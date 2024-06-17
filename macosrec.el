;;; macosrec.el --- Interface to macosrec binary to record screencasts and make screenshots.

;; Copyright (C) 2024 Pavel Popov
;; This package uses the MIT License.
;; See the LICENSE file.

;; Author: Pavel Popov <hotter-plazas-0x@icloud.com>
;; Version: 1.0
;; Package-Requires: ()

;;; Code:
(require 'cl-lib)

(defun macosrec-record-start ()
  "Start recording screencast of Emacs window."
  (interactive)
  (let ((window-id (thread-last
                     (process-lines "macosrec" "--list")
                     (cl-mapcar (lambda (p) (string-split p " ")))
                     (cl-find-if (lambda (p) (equal (nth 1 p) "Emacs")))
                     car)))
    (start-process "macosrec" nil "macosrec" "--record" window-id "--gif")
    (message "macosrec started recording window %s." window-id)))


(defun macosrec-screenshot ()
  "Do a screenshot of Start recording screencast of Emacs window."
  (interactive)
  (let ((window-id (thread-last
                     (process-lines "macosrec" "--list")
                     (cl-mapcar (lambda (p) (string-split p " ")))
                     (cl-find-if (lambda (p) (equal (nth 1 p) "Emacs")))
                     car)))
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


(define-key (current-global-map) (kbd "C-c s s") #'macosrec-screenshot)
(define-key (current-global-map) (kbd "C-c s v") #'macosrec-record-start)
(define-key (current-global-map) (kbd "C-c s c") #'macosrec-record-save)
(define-key (current-global-map) (kbd "C-c s k") #'macosrec-record-cancel)

(provide 'macosrec)
;;; macosrec.el ends here
