;;; macosrec.el --- Interface to macosrec binary to record screencasts and make screenshots.

;; Copyright (C) 2024 Pavel Popov
;; This package uses the MIT License.
;; See the LICENSE file.

;; Author: Pavel Popov <hotter-plazas-0x@icloud.com>
;; Version: 1.0
;; Package-Requires: (cl-lib)

;;; Code:
(require 'cl-lib)

(defvar macosrec-emacs-window-prefix "Emacs"
  "Prefix of Emacs window.")

(defun macosrec--window-id ()
  "Return window id for Emacs application."
  (thread-last
    (process-lines "macosrec" "--list")
    (cl-mapcar (lambda (p) (string-split p " ")))
    (cl-find-if (lambda (p) (equal (nth 1 p) macosrec-emacs-window-prefix)))
    car))

(defun macosrec-record-start ()
  "Start recording screencast of Emacs window."
  (interactive)
  (let ((window-id (macosrec--window-id)))
    (start-process "macosrec" nil "macosrec" "--record" window-id "--gif")
    (message "macosrec started recording window %s." window-id)))

(defun macosrec-screenshot ()
  "Do a screenshot of Start recording screencast of Emacs window."
  (interactive)
  (let ((window-id (macosrec--window-id)))
    (start-process "macosrec" nil "macosrec" "--screenshot" window-id)
    (message "macosrec asked to screenshot window %s." window-id)))

(defun macosrec-record-save ()
  "Stop recording screencast of Emacs window."
  (interactive)
  (shell-command "macosrec --save")
  (message "macosrec asked to save screencast."))

(defun macosrec-record-abort ()
  "Abort recording screencast of Emacs window."
  (interactive)
  (shell-command "macosrec --abort")
  (message "macosrec asked to abort screencast."))

(defvar macosrec-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") #'macosrec-screenshot)
    (define-key map (kbd "v") #'macosrec-record-start)
    (define-key map (kbd "c") #'macosrec-record-save)
    (define-key map (kbd "k") #'macosrec-record-abort)
    map)
  "Keymap for macosrec commands.")

(provide 'macosrec)
;;; macosrec.el ends here
