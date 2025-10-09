;;; gh.el --- Emacs wrapper for GitHub CLI  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Pavel Popov <hotter-plazas-0x@icloud.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (json "1.4"))
;; Keywords: tools, vc
;; URL: https://github.com/velppa/VELPA/tree/main/gh.el

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides an Emacs interface to the GitHub CLI (gh).
;; It allows you to browse and interact with GitHub pull requests
;; directly from Emacs.
;;
;; Main features:
;; - Browse pull requests with completion
;; - Open PRs in browser
;; - Filter by state (open/closed/all)
;; - Configurable PR limits
;;
;; Usage:
;;   M-x gh-browse-pr           ; Browse open PRs (default: 30)
;;   C-u M-x gh-browse-pr       ; Browse all PRs (up to 100)
;;   C-u 50 M-x gh-browse-pr    ; Browse 50 PRs of all states
;;
;; Requirements:
;; - GitHub CLI (gh) must be installed and authenticated
;; - See: https://cli.github.com/

;;; Code:

(require 'json)

(defgroup gh nil
  "Emacs wrapper for GitHub CLI."
  :group 'tools
  :prefix "gh-")

(defcustom gh-default-pr-limit 30
  "Default number of PRs to fetch."
  :type 'integer
  :group 'gh)

(defvar gh-repo-history nil
  "History of repository selections.")

(defun gh-list (kind repo &optional limit state)
  "List objects of KIND for REPO using gh CLI.
LIMIT specifies maximum number of PRs to fetch (default: `gh-default-pr-limit').
STATE can be \"open\", \"closed\", or \"all\" (default: \"open\")."
  (let ((cmd (format "gh %s list --json number,state,title,url,author --repo %s"
                     (symbol-name kind)
                     repo)))
    (when limit
      (setq cmd (format "%s --limit %d" cmd limit)))
    (when state
      (setq cmd (format "%s --state %s" cmd state)))
    (condition-case err
        (thread-last
          cmd
          shell-command-to-string
          json-read-from-string
          (seq-sort (lambda (a b)
                      (> (alist-get 'number a)
                         (alist-get 'number b)))))
      (error
       (user-error "Failed to list %ss: %s" (symbol-name kind) (error-message-string err))))))

;;;###autoload
(defun gh-browse-pr (&optional repo arg)
  "Open selected pull request from repository REPO in a browser.
With \\[universal-argument], fetch all PRs (up to 100).
With numeric prefix ARG, fetch that many PRs.

If REPO is not provided, then function prompts for a repository in the format \"owner/name\"
and maintains a history of previously accessed repositories."
  (interactive
   (list nil
         (cond
          ((null current-prefix-arg) gh-default-pr-limit)
          ((equal current-prefix-arg '(4)) 100)
          ((numberp current-prefix-arg) current-prefix-arg)
          (t gh-default-pr-limit))))
  (let* ((repo (or repo
                   (read-string "Repository (owner/name): "
                                (car gh-repo-history)
                                'gh-repo-history)))
         (limit (or arg gh-default-pr-limit))
         (state (if arg "all" "open"))
         (kind 'pr)
         (items (gh-list kind repo limit state))
         (items-alist
          (mapcar (lambda (item)
                    (let-alist item
                      (cons (if (equal state "all")
                                (format "#%-4s %-8s %-70s %s"
                                        .number
                                        (format "[%s]" .state)
                                        .title
                                        (if (equal (or .author.name "") "")
                                            .author.login
                                          (format "%s (%s)" .author.name .author.login)) )
                              (format "#%-4s %-70s %s"
                                      .number
                                      .title
                                      (if (equal (or .author.name "") "")
                                          .author.login
                                        (format "%s (%s)" .author.name .author.login))))
                            .url)))
                  items)))
    (unless items-alist
      (user-error "No %ss found for repository %s" (symbol-name kind) repo))
    (let* (;; (selection (completing-read
           ;;             (format "Browse PR (%d total): " (length items))
           ;;             items-alist nil t))
           (selection (completing-read
                       (format "Browse %s (%d total): " (symbol-name kind) (length items))
                       (lambda (string pred action)
                         (if (eq action 'metadata)
                             '(metadata (display-sort-function . identity))
                           (complete-with-action action items-alist string pred)))
                       nil t))
           (url (alist-get selection items-alist nil nil #'equal)))
      (when url
        (browse-url url)
        (message "Browsing %s: %s" (symbol-name kind) url)))))


;;;###autoload
(defun gh-browse-issue (arg)
  "Select Github repository, fetch issues, and open selected issue in the browser.
With \\[universal-argument], fetch all issues (up to 100).
With numeric prefix ARG, fetch that many issues.

The function prompts for a repository in the format \"owner/name\"
and maintains a history of previously accessed repositories."
  (interactive "P")
  (let* ((repo (read-string "Repository (owner/name): "
                            (car gh-repo-history)
                            'gh-repo-history))
         (limit (cond
                 ((null arg) gh-default-pr-limit)
                 ((equal arg '(4)) 100)
                 ((numberp arg) arg)
                 (t gh-default-pr-limit)))
         (state (if arg "all" "open"))
         (gh-issues (gh-list 'issue repo limit state))
         (issue-alist
          (mapcar (lambda (issue)
                    (let-alist issue
                      (cons (if (equal state "all")
                                (format "#%-4s %-8s %-70s %s"
                                        .number
                                        (format "[%s]" .state)
                                        .title
                                        (if (equal (or .author.name "") "")
                                            .author.login
                                          (format "%s (%s)" .author.name .author.login)) )
                              (format "#%-4s %-70s %s"
                                      .number
                                      .title
                                      (if (equal (or .author.name "") "")
                                          .author.login
                                        (format "%s (%s)" .author.name .author.login))))
                            .url)))
                  gh-issues)))
    (unless issue-alist
      (user-error "No issues found for repository %s" repo))
    (let* ((selection (completing-read
                       (format "Browse Issue (%d total): " (length gh-issues))
                       (lambda (string pred action)
                         (if (eq action 'metadata)
                             '(metadata (display-sort-function . identity))
                           (complete-with-action action issue-alist string pred)))
                       nil t))
           (url (alist-get selection issue-alist nil nil #'equal)))
      (when url
        (browse-url url)
        (message "Browsing Issue: %s" url)))))

(provide 'gh)

;;; gh.el ends here
