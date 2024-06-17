;;; my-eev.el --- My functions to work with eev package -*- lexical-binding: t; -*-
;; (require 'eev)
(require 'eev-load)
(require 'eev-aliases)

;;; Code

(defun ee-time-to-youtube-time (str)
  (save-match-data
    (cond ((string-match (rx (group (1+ digit)) ":"
                             (group (= 2 digit))) str)
	   (format "&t=%s" (+ (* 60  (string-to-number (match-string 1 str)))
                              (string-to-number (match-string 2 str))))))))


(defun find-youtube-video (id &optional time)
  (browse-url (format "http://yewtu.be/watch?v=%s%s"
	              id (or (ee-time-to-youtube-time (or time "")) ""))))


;; (find-bgprocess-ne `("/Applications/Safari.app/Contents/MacOS/Safari"))


(comment
 (ee-time-to-youtube-time "4:04") ;; "&t=244"
 )

(provide 'my-eev)
;;; my-eev.el ends here
