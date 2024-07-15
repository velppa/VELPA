;;; spark-sender.el --- Send region to spark-shell buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Pavel Popov

;; Author: Pavel Popov <hotter-plazas-0x@icloud.com>
;; URL: https://github.com/velppa/velpa/tree/main/spark-sender.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "29.4"))
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

;; This package allows flanges to be easily frobnicated.

;;;; Installation

;;;;; MELPA

;; If you installed from MELPA, you're done.

;;;;; Manual

;; Install these required packages:

;; + foo
;; + bar

;; Then put this file in your load-path, and put this in your init
;; file:

;; (require 'spark-sender)

;;;; Usage

;; Run one of these commands:

;; `spark-sender-command': Frobnicate the flange.

;;;; Tips

;; + You can customize settings in the `spark-sender' group.

;;;; Credits

;; This package would not have been possible without the following
;; packages: foo[1], which showed me how to bifurcate, and bar[2],
;; which takes care of flanges.
;;
;;  [1] https://example.com/foo.el
;;  [2] https://example.com/bar.el

;;; Code:

;;;; Requirements

(require 'foo)
(require 'bar)

;;;; Customization

(defgroup spark-sender nil
  "Settings for `spark-sender'."
  :link '(url-link "https://example.com/spark-sender.el"))

(defcustom spark-sender-something nil
  "This setting does something."
  :type 'something)

;;;; Variables

(defvar spark-sender-var nil
  "A variable.")

;;;;; Keymaps

;; This technique makes it easier and less verbose to define keymaps
;; that have many bindings.

(defvar spark-sender-map
  ;; This makes it easy and much less verbose to define keys
  (let ((map (make-sparse-keymap "spark-sender map"))
        (maps (list
               ;; Mappings go here, e.g.:
               "RET" #'spark-sender-RET-command
               [remap search-forward] #'spark-sender-search-forward
               )))
    (cl-loop for (key fn) on maps by #'cddr
             do (progn
                  (when (stringp key)
                    (setq key (kbd key)))
                  (define-key map key fn)))
    map))

;;;; Commands

;;;###autoload
(defun spark-sender-command (args)
  "Frobnicate the flange."
  (interactive)
  (spark-sender-foo
   (spark-sender--bar args)))

;;;; Functions

;;;;; Public

(defun spark-sender-foo (args)
  "Return foo for ARGS."
  (foo args))

;;;;; Private

(defun spark-sender--bar (args)
  "Return bar for ARGS."
  (bar args))

;;;; Footer

(provide 'spark-sender)

;;; spark-sender.el ends here
