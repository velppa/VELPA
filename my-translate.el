;;; my-translate.el -- my translation helper -*- lexical-binding: t; -*-

;; Copyright (C) 2023 by Pavel Popov

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


;;; Code:

(require 'reverso)
(require 'posframe)

(defun my-translate-dutch->english (start end)
  "Translate from Dutch to English."
  (interactive "r")
  (let ((input (buffer-substring start end)))
    ;; (message "input: %s" input)
    (reverso--translate
     input 'dutch 'english
     (lambda (data)
       (reverso--with-buffer
         (reverso--translate-render input data))))))


(defvar my-translate-log '() "Log of translation pairs.")


(defun my-translate-word-dutch->english (&optional)
  "Translate word under cursor from Dutch to English."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (input (buffer-substring-no-properties (car bounds) (cdr bounds))))
    ;; (message "input: %s" input)
    (reverso--translate
     input 'dutch 'english
     (lambda (data)
       (let ((translation (alist-get :translation data)))
         (add-to-list 'my-translate-log `(,input . ,translation))
         (when (posframe-workable-p)
           (posframe-show " *translation*"
                          :string translation
                          :border-color "#ff0000"
                          :internal-border-width 1
                          :position (point))))))))

(provide 'my-translate)

;;; my-translate.el ends here
