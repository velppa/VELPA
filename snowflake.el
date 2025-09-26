;;; snowflake.el --- Utilities to work with Snowflake  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pavel Popov

;; Author: Pavel Popov <hotter-plazas-0x@icloud.com>
;; URL: https://example.com/snowflake.el
;; Version: 0.1-pre
;; Keywords: snowflake

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

;; This package is a wrapper around "snow" CLI utility to work with Snowflake.

;;;; Installation

;;; Code:

;;;; Requirements

;; (require 'foo)
;; (require 'bar)

;;;; Functions

(cl-defun snow-sql (beg end)
  (interactive "r")
  (let ((sql (buffer-substring-no-properties beg end))
        (temp-file ))
    (async-shell-command "snow sql -f "))
  )

(defun snow-sql ()
  "Runs current region with 'snow sql'."
  (interactive)
  (if (use-region-p)
      (let ((temp-file (make-temp-file "snow-sql-query-" nil ".sql")))
        (write-region (region-beginning) (region-end) temp-file)
        (with-environment-variables (("COLUMNS" "200"))
            (async-shell-command (format "snow sql --format=table -f %s" temp-file) "*snow-sql*")))
    (error "No active region")))

;;;; Footer

(provide 'snowflake)

;;; snowflake.el ends here
