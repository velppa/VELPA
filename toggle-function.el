;;; toggle-function.el --- Helper to create a toggle function.    -*- coding: utf-8; lexical-binding: t -*-
;;
;; Filename: toggle-function.el
;; Description: Helper to create a toggle function.
;; Author: Pavel Popov <pavelpopov@outlook.com>
;; Copyright (C) 2021, Pavel Popov, all rights reserved.
;; Created: Fri Feb 27 20:32:14 2009 (-0800)
;; Version: 0
;; Package-Requires: ()
;; URL: https://www.reddit.com/r/emacs/comments/l4v1ux/
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
;; (setq lexical-binding t)

(defun toggle-function
  (buffer-name buffer-create-fn &optional switch-cont)
  "Makes a toggle-function to have raise-or-create behaviour.

Creates a toggle-function that executes BUFFER-CREATE-FN if a
buffer named BUFFER-NAME doesn't exist, switches to the buffer
named BUFFER-NAME if it exists, and switches to the previous
buffer if we are currently visiting buffer BUFFER-NAME.

The SWITCH-CONT argument is a function which, if given, is called
after the buffer has been created or switched to.  This allows
running further actions that setup the state of the buffer or
modify it."
  (lambda ()
    (interactive)
    (let ((target-buf (get-buffer buffer-name)))
     (if target-buf
	 (if (eq (current-buffer) target-buf)
	     (progn
	       (message "switching back...")
	       (switch-to-buffer nil))
	     (progn
	       (message "switching to target buffer")
	       (switch-to-buffer buffer-name)
	       (when switch-cont (funcall switch-cont))))
       (message "creating buffer...")
       (funcall buffer-create-fn)
       (when switch-cont (funcall switch-cont))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'toggle-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; toggle-function.el ends here
