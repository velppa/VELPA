;;; datadog.el --- Utilities to work with DataDog  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Pavel Popov

;; Author: Pavel Popov <hotter-plazas-0x@icloud.com>
;; URL: https://github.com/velppa/velpa/tree/main/datadog.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.2"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides utilities to work with Datadog.


;;; Code:

(require 'ts)
(require 'plz)

;;;; Variables

(defvar datadog-api-key
  (getenv "DD_API_KEY")
  "Api key to authenticate in DataDog.  Needed only for metrics.")

(defvar datadog-app-key
  (getenv "DD_APP_KEY")
  "Application key to authenticate in DataDog.  Needed only for metrics.")

(defvar datadog-notebooks
  (list
   :temporary 0
   :persistent 0)
  "Plist of Notebook IDs to use in metrics.")

(defvar datadog-metrics-default-span
  "4h" "Default span Datadog metrics.
Possible values: 1m, 5m, 10m, 15m, 30m, 1h, 4h, 1d, 2d, 1w, 1mo,
3mo, 6mo, week_to_date, month_to_date, 1y, alert")

(defvar datadog-traces-columns
  (string-join '("service"
                 "resource_name"
                 "@duration"
                 "@http.method"
                 "@http.status_code"
                 "@_span.count"
                 "@_duration.by_service"
                 "@error.message"
                 "operation_name"
                 "@http.url")
               ",")
  "Traces columns.")

(defvar datadog-default-from
  '(day -14)
  "Default value for FROM timestamp.")

;;;; Commands

(defun datadog--browse-url (prefix params)
  (thread-last
    (mapconcat
     (lambda (x) (pcase-let ((`(,k ,v) x)) (format "%s=%s" k (url-hexify-string (format "%s" v)))))
     params "&")
    (format prefix)
    browse-url))

(cl-defun datadog--get-time-bounds (&key from to)
  "Return PLIST with keys :from and :to for time bounds."
  (let* ((milliseconds (lambda (x) (thread-last x ts-unix truncate (* 1000))))
         (to (or to (ts-now)))
         (from (or from datadog-default-from)))
    (list
     :from (funcall milliseconds (apply #'ts-adjust (seq-concatenate 'list from `(,to))))
     :to (funcall milliseconds to))))

(cl-defun datadog--get-time-bounds-datetime (&key from to)
  "Return PLIST with keys :from and :to for time bounds as datetime string."
  (let* ((fmt (lambda (x) (ts-format "%Y-%m-%dT%H:%M:%S%z" x)))
         (to (or to (ts-now)))
         (from (or from datadog-default-from)))
    (list
     :from (funcall fmt (apply #'ts-adjust (seq-concatenate 'list from `(,to))))
     :to (funcall fmt to))))

(comment
 (datadog--get-time-bounds)
 ;; (:from 1729516835000 :to 1730730035000)
 (datadog--get-time-bounds :from nil :to nil)
 ;; (:from 1729516856000 :to 1730730056000)

 (datadog--get-time-bounds-datetime)
 ;; (:from "2024-10-21T15:36:05+0200" :to "2024-11-04T15:36:05+0100")

 (datadog--get-time-bounds)
 ;; (:from 1729516862000 :to 1730730062000)

 (datadog--get-time-bounds :from '(day -2) :to (ts-adjust 'day -1 (ts-now)))
 ;; (:from 1730470866000 :to 1730643666000)
 ;; (:from 1730470765000 :to 1730643565000)
 ;; (:from 1730557058000 :to 1730729858000)

 (apply #'ts-adjust (seq-concatenate 'list '(day -14) `(,(ts-now))))
 ;; #s(ts nil nil nil nil nil nil nil nil nil nil nil ...)

 (datadog--get-time-bounds :from '(hour -2))
 ;; (:from 1730722887000 :to 1730730087000)

 (pcase-let
     ((`(:from ,from :to ,to) (datadog--get-time-bounds :from '(hour -2))))
   (message "from: %s, to: %s" from to))
 ;; "from: 1730722917000, to: 1730730117000"
 )
;;;###autoload

(cl-defun datadog-browse-logs (query &key from to)
  "Browse logs for QUERY using ARGS plist with optional parameters."
  (interactive "sQuery: ")
  (cl-destructuring-bind
        (&key from to)
      (datadog--get-time-bounds :from from :to to)
    (let* ((cols (or (plist-get args :columns) "host,service"))
           (params `((live "true")
                     (viz "stream")
                     (from_ts ,from)
                     (to_ts ,to)
                     (stream_sort "desc")
                     (storage "hot")
                     (refresh_mode "sliding")
                     (messageDisplay "inline")
                     (fromUser "true")
                     (cols ,cols)
                     (query ,(thread-last query (string-replace "[" "\\[") (string-replace "]" "\\]")))
                     (agg_t "count")
                     (agg_m_source "base")
                     (agg_m "count"))))
      (datadog--browse-url "https://app.datadoghq.com/logs?%s" params))))

(defun datadog-browse-events (query &rest args)
  "Browse events for QUERY.  ARGS is a plist with setting options for the query."
  (interactive "sQuery: ")
  (cl-destructuring-bind
      (&key from to)
      (datadog--get-time-bounds :from from :to to)
    (let* ((cols (or (plist-get args :columns) "host,service"))
           (params
            `((live "false")
              (from_ts ,from)
              (to_ts ,to)
              (sort "DESC")
              (refresh_mode "paused")
              (options "")
              (messageDisplay "expanded-lg")
              (cols "")
              (query ,query))))
      (datadog--browse-url "https://app.datadoghq.com/event/explorer?%s" params))))

(cl-defun datadog-browse-traces (query &key from to (columns datadog-traces-columns))
  "Browse traces for QUERY.  ARGS is a plist with setting options for the query."
  (interactive "sQuery: ")
  (cl-destructuring-bind
      (&key from to)
      (datadog--get-time-bounds :from from :to to)
    (let ((params
           `((paused "false")
             (start ,start)
             (end ,end)
             (view "spans")
             (spanType "all")
             (sort "time")
             (shouldShowLegend "true")
             (query_translation_version "v0")
             (messageDisplay "inline")
             (historicalData "true")
             (graphType "flamegraph")
             (fromUser "false")
             (cols ,columns)
             (agg_t "count")
             (agg_m_source "base")
             (agg_m "count")
             (query ,query))))
      (datadog--browse-url "https://app.datadoghq.com/apm/traces?%s" params))))

(defun datadog-pods (query &rest args)
  (let* ((params
          `((panel_tab "yaml")
            (explorer-na-groups "false")
            (query ,query))))
    (datadog--browse-url
     "https://app.datadoghq.com/orchestration/explorer/pod?%s"
     params)))

(cl-defun datadog-nodes (query &key (groups "label#eks.vio.com/nodegroup"))
  (let* ((params
          `((panel_tab "yaml")
            (groups ,groups)
            (explorer-na-groups "false")
            (query ,query))))
    (datadog--browse-url
     "https://app.datadoghq.com/orchestration/explorer/node?%s"
     params)))

(defun datadog--get-notebook (id)
  (plz 'get (format "https://api.datadoghq.com/api/v1/notebooks/%s" id)
    :headers `(("Content-Type" . "application/json")
              ("DD-API-KEY" . ,datadog-api-key)
              ("DD-APPLICATION-KEY" . ,datadog-app-key))
    :as #'json-read))

(cl-defun datadog-metrics-browse (&key notebook-id notebook)
  (let ((id (or notebook-id
                (plist-get datadog-notebooks notebook)
                (plist-get datadog-notebooks :persistent))))
    (browse-url (format "https://app.datadoghq.com/notebook/%s/datadog-el" id))))

(cl-defun datadog-metrics
    (&key queries from to formulas description
          notebook-id notebook
          overwrite
          skip-browse
          (display-type "line")
          (graph-size "xl")
          (span datadog-metrics-default-span))
  "Create a new cell in a notebook with the provided queries and browse the notebook."
  (let* ((notebook-id (or notebook-id
                          (plist-get datadog-notebooks notebook)
                          (plist-get datadog-notebooks :persistent)))
         (current (thread-last (datadog--get-notebook notebook-id)
                               (alist-get 'data)
                               (alist-get 'attributes)))
         (request (thread-last
                    `((display_type . ,display-type)
                      (response_format . "timeseries")
                      (queries . ,(cl-mapcar
                                   (lambda (query idx)
                                     `((data_source . "metrics")
                                       (name . ,(format "%c" idx))
                                       (query . ,query)))
                                   queries
                                   (range-uncompress '(?a . ?z))))
                      (formulas . ,(mapcar
                                    (lambda (x)
                                      `((formula . ,(plist-get x :formula))
                                        (alias . ,(plist-get x :alias))))
                                    formulas))
                      (style . ((line_type . "solid")
                                (line_width . "normal")
                                (palette . "dog_classic"))))
                    (rassq-delete-all nil)))
         (time (if (or from to)
                   (cl-destructuring-bind
                       (&key from to)
                       (datadog--get-time-bounds-datetime :from from :to to)
                     `((start . ,from) (end . ,to)))
                 `((live_span . ,span))))
         (cell `((attributes . ((definition . ((requests . (,request))
                                               (show_legend . t)
                                               (type . "timeseries")
                                               (yaxis . ((scale . "linear")))))
                                (time . ,time)
                                (graph_size . ,graph-size)))
                 (type . "notebook_cells")))
         (description-cell `((attributes . ((definition . ((type . "markdown") (text . ,description)))))
                             (type . "notebook_cells")))
         (cells (if description (list description-cell cell) (list cell)))
         (new-cells (if overwrite cells
                      (let ((old-cells (alist-get 'cells current)))
                        (cl-concatenate 'list cells old-cells))))
         (body `((data . ((attributes . ((cells . ,new-cells)
                                         (name . ,(or (alist-get 'name current) "datadog.el metrics"))
                                         (time . ,(if overwrite time
                                                    (alist-get 'time current)))))
                          (type . "notebooks"))))))
    ;; (json-encode body)
    (plz 'put (format  "https://api.datadoghq.com/api/v1/notebooks/%s" notebook-id)
      :headers `(("Content-Type" . "application/json")
                 ("DD-API-KEY" . ,datadog-api-key)
                 ("DD-APPLICATION-KEY" . ,datadog-app-key))
      :body (json-encode body)
      :as #'json-read
      :then (lambda (_)
              (unless skip-browse
                (browse-url (format "https://app.datadoghq.com/notebook/%s/datadog-el" notebook-id))))
      :else (lambda (err)
              (message "Request failed, details in *datadog-errors* buffer")
              (with-output-to-temp-buffer "*datadog-errors*"
                (princ (format "Body: %s\n" (json-encode body)))
                (princ (format "Error: %s" err))) ))))


(defun datadog-metric (query)
  "Plot metric from QUERY in Datadog temporary notebook."
  (interactive "sQuery: ")
  (datadog-metrics :queries `(,query) :notebook :temporary :overwrite t))

;;;; Footer

(provide 'datadog)

;;; datadog.el ends here
