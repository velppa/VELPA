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

;; Then put this file in your load-path, and put this in your init
;; file:

;; (require 'spark-sender)

;;; Code:

;;;;; Variables
(defvar spark-sender-target-buffer-name "*spark-shell*"
  "The name of the target buffer.")

;;;;; Keymaps

;; This technique makes it easier and less verbose to define keymaps
;; that have many bindings.

;; (defcustom spark-sender-mode-prefix "C-c s"
;;   "Prefix for spark-sender-mode keybindings."
;;   :type 'key-sequence
;;   :group 'spark-sender)

;; (defun spark-sender-setup-keybindings ()
;;   "Set keybindings for `spark-sender-mode'."
;;   (let ((prefix spark-sender-mode-prefix))
;;     (dolist (binding '(("r" . spark-sender-send-region)
;;                        ("w" . spark-sender-send-region-wrapped)
;;                        ("v" . spark-sender-send-region-val-wrapped)))
;;       (let ((key (concat prefix " " (car binding))))
;;         (keymap-local-set key (cdr binding))))))

;; (define-minor-mode spark-sender-mode
;;   "A minor mode to send code using Spark Sender."
;;   :lighter " SparkSender"
;;   :group 'spark-sender
;;   (if spark-sender-mode
;;       (spark-sender-setup-keybindings)))

;; (define-globalized-minor-mode spark-sender-global-mode spark-sender-mode
  ;; (lambda () (spark-sender-mode 1)))

;;;; Functions

;;;;; Private

(defun spark-sender--wrap-val-in-curly-braces (input-str)
  "Wrap the <code> part in curly braces.

The wrapping happens only if the input string starts with `val
<name> = <code>'."
  (if (string-match "val \\([[:alnum:]]+\\) =\\(\\(.\\|\n\\)*\\)" input-str)
      (let ((name (match-string 1 input-str))
            (code (match-string 2 input-str)))
        (format "val %s = { %s }" name code))
    input-str))

(defun spark-sender--wrap-in-curly-braces (input-str)
  "Wrap INPUT-STR in curly braces."
  (format "{ %s }" input-str))

;;;;; Public

;;;; Commands
;;;###autoload
(defun spark-sender-send (beg end arg)
  "Send current region to `spark-sender-target-buffer-name' buffer.

With one universal argument (C-u), wrap val in curly braces.
With two universal arguments (C-u C-u), wrap region in curly braces."
  (interactive "r\nP")
  (let ((wrapper (cond ((null arg) #'identity)
                       ((equal arg '(4)) #'spark-sender--wrap-val-in-curly-braces)
                       ((equal arg '(16)) #'spark-sender--wrap-in-curly-braces))))
    (message "Sent %s chars to %s using %s wrapper" (- end beg) spark-sender-target-buffer-name wrapper)
    (-->
     (buffer-substring-no-properties beg end)
     ;; (split-string it (rx eol)) (mapcar (lambda (s) (format "%s " s)) it) (string-join it)
     (funcall wrapper it)
     (format "%s \n" it)
     ;; (message "lines: %s" it)
     (comint-send-string spark-sender-target-buffer-name it)
     )))


(defun spark-sender-build-sender (target-buffer)
  "Returns function that invokes `spark-sender-send` to TARGET-BUFFER."
  (lambda (beg end arg)
    (interactive "r\nP")
    (let ((spark-sender-target-buffer-name target-buffer))
      (spark-sender-send beg end arg))))


;;;###autoload
;; (defun spark-sender-send-region (beg end)
;;   "Send current region to *spark-shell* buffer."
;;   (interactive "r")
;;   (thread-last
;;     (buffer-substring-no-properties beg end)
;;     (format "%s\n")
;;     (comint-send-string spark-sender-target-buffer-name)))

;;;###autoload
;; (defun spark-sender-send-region-val-wrapped (beg end)
;;   "Send current region wrapped into closure to *spark-shell* buffer"
;;   (interactive "r")
;;   (thread-last
;;     (buffer-substring-no-properties beg end)
;;     spark-sender--wrap-val-in-curly-braces
;;     (format "%s\n")
;;     (comint-send-string spark-sender-target-buffer-name)))

;;;###autoload
;; (defun spark-sender-send-region-wrapped (beg end)
;;   "Send current region wrapped into closure to *spark-shell* buffer"
;;   (interactive "r")
;;   (thread-last
;;     (buffer-substring-no-properties beg end)
;;     spark-sender--wrap-in-curly-braces
;;     (format "%s\n")
;;     (comint-send-string spark-sender-target-buffer-name)))

;;;; Footer
(provide 'spark-sender)

;;; spark-sender.el ends here
