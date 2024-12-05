;;; macos-keychain.el --- Operating with internet passwords in macOS Keychain -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Pavel Popov

;; Author: Pavel Popov <hotter-plazas-0x@icloud.com>
;; URL: https://github.com/velpa/VELPA/tree/main/macos-keychain.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "29.1"))
;; Keywords: macos

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

;; This package provides functions to operate on passwords in macOS
;; keychain.

;;;; Installation

;;;;; Manual

;; Put this file in your load-path, and put this in your init
;; file:

;; (require 'macos-keychain)

;;; Code:

;;;; Functions

;;;;; Public

(cl-defun mac-keychain-add-internet-password (&key machine login password comment)
  "Adds internet password to macOS keychain."
  (let* ((args
	  (thread-last
	    `(("-s" . ,machine)
	      ("-a" . ,login)
              ("-j" . ,comment)
	      ("-w" . ,password))
	    (rassq-delete-all nil)
	    (mapcan (lambda (x) `(,(car x) ,(cdr x))))))
	 (res (with-output-to-string
		(with-current-buffer standard-output
		  (apply
		   #'call-process
		   "security"
		   nil (current-buffer) nil
		   "add-internet-password"
		   "-U"
		   args)))))
      (if (equal res "") t (user-error res))))

(cl-defun mac-keychain-delete-internet-password (&key machine login)
  "Deletes internet password from macOS keychain."
  (let* ((args
	  (thread-last
	    `(("-s" . ,machine)
	      ("-a" . ,login))
	    (rassq-delete-all nil)
	    (mapcan (lambda (x) `(,(car x) ,(cdr x)))))))
    (with-output-to-string
      (with-current-buffer standard-output
	(apply
	 #'call-process
	 "security"
	 nil (current-buffer) nil
	 "delete-internet-password"
	 args)))))

;;;; Footer

(provide 'macos-keychain)

;;; macos-keychain.el ends here
