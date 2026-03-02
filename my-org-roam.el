;;; my-org-roam.el --- Utilities for org-roam  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pavel Vasilyev

;; Author: Pavel Vasilyev
;; URL: https://github.com/velppa/VELPA
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.2") (org-roam "2.0"))
;; Keywords: org, roam

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

;; This package provides utilities for working with org-roam.

;;;; Installation

;;;;; Manual

;; Install these required packages:

;; + org-roam

;; Then put this file in your load-path, and put this in your init
;; file:

;; (require 'my-org-roam)

;;;; Usage

;; Run one of these commands:

;; `my-org-roam-backlinks-table': Generate an Org Mode table of backlinks
;; to a node with heading names and TODO status.

;;; Code:

;;;; Requirements

(require 'org-roam)

;;;; Customization

(defgroup my-org-roam nil
  "Settings for `my-org-roam'."
  :link '(url-link "https://github.com/velppa/VELPA")
  :group 'org-roam)

;;;; Functions

;;;;; Public

(defun my-org-roam-backlinks-table (node-title)
  "Return an Org Mode table of backlinks to NODE-TITLE with heading names and TODO status."
  (let* ((node-id (caar (org-roam-db-query
                         [:select [id] :from nodes :where (= title $s1)]
                         node-title)))
         ;; Get source IDs and their positions
         (links-data (org-roam-db-query
                      [:select [source pos] :from links :where (= dest $s1)]
                      node-id))
         ;; Get node info for sources
         (source-ids (mapcar #'car links-data))
         (backlinks (org-roam-db-query
                     [:select [file title id]
                      :from nodes
                      :where (in id $v1)]
                     (vconcat source-ids)))
         (results '()))
    ;; Collect heading and TODO info for each backlink
    (dolist (backlink backlinks)
      (let* ((file (nth 0 backlink))
             (title (nth 1 backlink))
             (node-id-back (nth 2 backlink))
             ;; Find position for this node-id
             (pos (cadr (assoc node-id-back links-data)))
             (heading "")
             (todo-state ""))
        (when (and file (file-exists-p file))
          (with-current-buffer (find-file-noselect file)
            (save-excursion
              (goto-char (or pos (point-min)))
              (when (ignore-errors (org-back-to-heading t))
                (setq heading (org-get-heading t t t t))
                (setq todo-state (or (org-get-todo-state) ""))))))
        (push (list title heading todo-state) results)))
    ;; Format as org table
    (with-temp-buffer
      (insert "| File | Heading | TODO |\n")
      (insert "|------+---------+------|\n")
      (dolist (row (nreverse results))
        (insert (format "| %s | %s | %s |\n"
                       (or (nth 0 row) "")
                       (or (nth 1 row) "")
                       (or (nth 2 row) ""))))
      (buffer-substring-no-properties (point-min) (point-max)))))

;;;; Footer

(provide 'my-org-roam)

;;; my-org-roam.el ends here
