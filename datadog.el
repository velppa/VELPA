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

;;;; Commands

(defun datadog--browse-url (prefix params)
  (thread-last
    (mapconcat (lambda (x)
                 (pcase-let ((`(,k ,v) x))
                   (format "%s=%s" k (url-hexify-string (format "%s" v)))))
               params "&")
    (format prefix)
    browse-url))

(defun datadog--get-time-bounds (args)
  "Return PLIST with keys :from and :to for time bounds."
  (message "args: %s" args)
  (let* ((to (or (plist-get args :to) (ts-now)))
         (from (append (or (plist-get args :from) '(day -14)) `(,to)))
         (milliseconds (lambda (x) (thread-last x ts-unix truncate (* 1000)))))
    (list
     :from (funcall milliseconds (apply #'ts-adjust from))
     :to (funcall milliseconds to))))

(comment
 (datadog--get-time-bounds)
 ;; (:from 1727085975000 :to 1728295575000)
 (datadog--get-time-bounds '(:from hour -2))
 ;; (:from 1727088040000 :to 1728297640000)
 ;; (:from 1728289907000 :to 1728297107000)
 ;; (:from-ts 1728287370 :to-ts 1728294570)
 (pcase-let
     ((`(:from ,from :to ,to) (datadog--get-time-bounds '(:from (hour -2)))))
   (message "from: %s, to: %s" from to))
 ;; "from: 1727086800000, to: 1728296400000"
 )

;;;###autoload
(defun datadog-browse-logs (query &rest args)
  "Browse logs for QUERY using ARGS plist with optional parameters."
  (interactive "sQuery: ")
  (pcase-let
      ((`(:from ,from :to ,to) (datadog--get-time-bounds args)))
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
                     (query ,(thread-last
                               query
                               (string-replace "[" "\\[")
                               (string-replace "]" "\\]")))
                     (agg_t "count")
                     (agg_m_source "base")
                     (agg_m "count"))))
      (datadog--browse-url
       "https://app.datadoghq.com/logs?%s"
       params))))

(defun datadog-browse-events (query &rest args)
  "Browse events for QUERY.  ARGS is a plist with setting options for the query."
  (interactive "sQuery: ")
  (pcase-let
      ((`(:from ,from :to ,to) (datadog--get-time-bounds args)))
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
      (datadog--browse-url
       "https://app.datadoghq.com/event/explorer?%s"
       params))))

(defun datadog-browse-traces (query &rest args)
  "Browse traces for QUERY.  ARGS is a plist with setting options for the query."
  (interactive "sQuery: ")
  (pcase-let
      ((`(:from ,from :to ,to) (datadog--get-time-bounds args)))
    (let* ((cols (or (plist-get args :columns) "service,resource_name,@duration,@http.method,@http.status_code,@_span.count,@_duration.by_service,@error.message,operation_name,@http.url"))
           (params
            `((paused "false")
              (start ,from)
              (end ,to)
              (view "spans")
              (spanType "all")
              (sort "time")
              (shouldShowLegend "true")
              (query_translation_version "v0")
              (messageDisplay "inline")
              (historicalData "true")
              (graphType "flamegraph")
              (fromUser "false")
              (cols ,cols)
              (agg_t "count")
              (agg_m_source "base")
              (agg_m "count")
              (query ,query))))
      (datadog--browse-url
       "https://app.datadoghq.com/apm/traces?%s"
       params))))

(defun datadog-pods (query &rest args)
  (let* ((params
          `((panel_tab "yaml")
            (explorer-na-groups "false")
            (query ,query))))
    (datadog--browse-url
     "https://app.datadoghq.com/orchestration/explorer/pod?%s"
     params)))

(defun datadog-nodes (query &rest args)
  (pcase-let
      ((`(:groups ,groups) args))
    (let* ((params
            `((panel_tab "yaml")
              (groups ,(or groups "label#eks.vio.com/nodegroup"))
              (explorer-na-groups "false")
              (query ,query))))
      (datadog--browse-url
       "https://app.datadoghq.com/orchestration/explorer/node?%s"
       params))))

;;;; Footer

(provide 'datadog)

;;; datadog.el ends here
