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

;;;###autoload
(defun datadog-browse-logs (query &optional cols days)
  "Browse logs for QUERY for past DAYS, exposing columns COLS."
  (interactive "sQuery: ")
  (let* ((days (or days 14))
         (cols (or cols "host,service"))
         (to-ts (time-convert (current-time) 'integer))
         (from-ts (time-convert (time-add to-ts (* days -24 60 60)) 'integer))
         (params `((live "true")
                   (viz "stream")
                   (from_ts ,(* from-ts 1000))
                   (to_ts ,(* to-ts 1000))
                   (stream_sort "desc")
                   (storage "hot")
                   (refresh_mode "sliding")
                   (messageDisplay "inline")
                   (fromUser "true")
                   (cols ,cols)
                   (query ,(thread-last
                             query
                             (string-replace "[" "\\[")
                             (string-replace "]" "\\]")
                             url-encode-url))
                   (agg_t "count")
                   (agg_m_source "base")
                   (agg_m "count")))
         (param-str (mapconcat (lambda (x) (format "%s=%s" (car x) (cadr x))) params "&")))
    (browse-url (format "https://app.datadoghq.com/logs?%s" param-str))))

(defun datadog-browse-events (query &optional cols days)
  "Browse events for QUERY for past DAYS, exposing columns COLS."
  (interactive "sQuery: ")
  (let* ((days (or days 14))
         (cols (or cols "host,service"))
         (to-ts (time-convert (current-time) 'integer))
         (from-ts (time-convert (time-add to-ts (* days -24 60 60)) 'integer))
         (params
          `((live "false")
            (from_ts ,(* from-ts 1000))
            (to_ts ,(* to-ts 1000))
            (sort "DESC")
            (refresh_mode "paused")
            (options "")
            (messageDisplay "expanded-lg")
            (cols "")
            (query ,(thread-last
                      query
                      url-encode-url))))
         (param-str (mapconcat (lambda (x) (format "%s=%s" (car x) (cadr x))) params "&")))
    (browse-url (format "https://app.datadoghq.com/event/explorer?%s" param-str))))

(defun datadog-browse-traces (query &optional cols days)
  "Browse traces for QUERY for past DAYS, exposing columns COLS."
  (interactive "sQuery: ")
  (let* ((days (or days 14))
         (cols (or cols "service,resource_name,@duration,@http.method,@http.status_code,@_span.count,@_duration.by_service,@error.message,operation_name,@http.url"))
         (to-ts (time-convert (current-time) 'integer))
         (from-ts (time-convert (time-add to-ts (* days -24 60 60)) 'integer))
         (params
          `((paused "false")
            (start ,(* from-ts 1000))
            (end ,(* to-ts 1000))
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
            (query ,(thread-last
                      query
                      url-encode-url))))
         (param-str (mapconcat (lambda (x) (format "%s=%s" (car x) (cadr x))) params "&")))
    (browse-url (format "https://app.datadoghq.com/apm/traces?%s" param-str))))


;;;; Functions

;;;;; Public

(defun datadog-foo (args)
  "Return foo for ARGS."
  (foo args))

;;;;; Private

(defun datadog--bar (args)
  "Return bar for ARGS."
  (bar args))

;;;; Footer

(provide 'datadog)

;;; datadog.el ends here
