;;; buffer-flip.el --- Cycle through buffers like Alt-Tab in Windows

;; Author: Russell Black (killdash9@github)
;; Keywords: convenience
;; URL: https://github.com/killdash9/buffer-flip.el
;; Created: 10th November 2015
;; Package-Version: 20220718.10
;; Package-Revision: dda0cbcd202c
;; Package-Requires: ((cl-lib "0.5"))

;; Patched: Pavel Popov (hotter-plazas-0x@icloud.com)
;; Date: <2026-02-16 Mon>

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
;; Inspired by Alt-Tab.  Quickly flip through recently-used buffers.

;;; Code:
(require 'cl-lib)

(defvar buffer-flip-original-window-configuration nil
  "Saves the current window configuration when flipping begins.
Used by `buffer-flip-abort' to restore the original buffer.")

(defcustom buffer-flip-skip-patterns nil
  "A list of regular expressions.
Buffers with names matching these patterns will be skipped when
flipping through buffers."
  :type '(repeat string) :group 'buffer-flip)

;;;###autoload
(defun buffer-flip (&optional arg original-configuration)
  "Begin cycling through buffers.
With prefix ARG, invoke `buffer-flip-other-window'.
ORIGINAL-CONFIGURATION is used internally by
`buffer-flip-other-window' to specify the window configuration to
be restored upon abort."
  (interactive "P")
  ;; ensure current buffer is on the top of the stack at outset.  It's
  ;; rare, but this happens sometimes, particularly with the help
  ;; buffer.
  (switch-to-buffer (current-buffer))
  (if arg
      (buffer-flip-other-window)    ; C-u calls buffer-flip-other-window
    (setq buffer-flip-original-window-configuration ;restored in abort
          (or original-configuration (current-window-configuration)))
    (buffer-flip-cycle 'forward)    ; flip forward
    ))

;;;###autoload
(defun buffer-flip-other-window ()
  "Switch to another window and begin cycling through buffers in that window.
If there is no other window, one is created first."
  (interactive)
  (let ((original-window-configuration (current-window-configuration)))
    (when (= 1 (count-windows))
      (split-window-horizontally))
    (other-window 1)
    (buffer-flip nil original-window-configuration)))

(defun buffer-flip-cycle (&optional direction)
  "Cycle in the direction indicated by DIRECTION.
DIRECTION can be 'forward or 'backward"
  (let ((l (buffer-list)))
    (switch-to-buffer            ; Switch to next/prev buffer in stack
     (cl-do ((buf (current-buffer)     ; Using the current buffer as a
                  (nth (mod (+ (cl-position buf l) ; reference point to cycle
                               (if (eq direction 'backward) -1 1)) ; fwd or back
                            (length l)) l)) ; Mod length to wrap
             (count (length l) (1- count))) ; count the number of iterations
         ((or (= 0 count) ;; don't cycle through list more than once.
              (not (buffer-flip-skip-buffer buf))) buf)) t))) ; skip some buffers

(defun buffer-flip-skip-buffer (buf)
  "Return non-nil if BUF should be skipped."
  (or (get-buffer-window buf)         ; already visible?
      (= ? (elt (buffer-name buf) 0)) ; internal?
      (let ((name (buffer-name buf))) ; matches regex?
        (cl-find-if (lambda (rex) (string-match-p rex name) )
                    buffer-flip-skip-patterns))))

(defun buffer-flip-forward ()
  "Switch to previous buffer during cycling.
This command should be bound to a key inside of
`buffer-flip-map'."
  (interactive)
  (buffer-flip-cycle 'forward))

(defun buffer-flip-backward ()
  "Switch to previous buffer during cycling.
This command should be bound to a key inside of
`buffer-flip-map'."
  (interactive)
  (buffer-flip-cycle 'backward))

(defun buffer-flip-abort ()
  "Abort buffer cycling process and return to original buffer.
This command should be bound to a key inside of
`buffer-flip-map'."
  (interactive)
  (set-window-configuration buffer-flip-original-window-configuration)
  (call-interactively #'keyboard-quit))

(provide 'buffer-flip)
;;; buffer-flip.el ends here
