;;; my-eev.el --- My functions to work with eev package -*- lexical-binding: t; -*-
;; (require 'eev)
(require 'eev-load)
(require 'eev-aliases)
(require 'eev-videolinks)

;;; Code

(defun ee-time-to-youtube-time (str)
  (save-match-data
    (cond ((string-match (rx (group (1+ digit)) ":"
                             (group (= 2 digit))) str)
	   (format "&t=%s" (+ (* 60  (string-to-number (match-string 1 str)))
                              (string-to-number (match-string 2 str))))))))


;; (defun find-youtube-video (id &optional time)
;;   (browse-url (format "http://yewtu.be/watch?v=%s%s"
;; 	              id (or (ee-time-to-youtube-time (or time "")) ""))))


(setq ee-find-eev-video-function 'find-eevyoutube-video)
;; (setq ee-find-youtube-video-program 'find-firefox)
(setq ee-find-youtube-video-program 'browse-url)

;; (find-bgprocess-ne `("/Applications/Safari.app/Contents/MacOS/Safari"))
;; (ee-find-youtube-video "hOAqBc42Gg8" "6:25")

(comment
 (ee-time-to-youtube-time "4:04") ;; "&t=244"
 )

(provide 'my-eev)
;;; my-eev.el ends here
