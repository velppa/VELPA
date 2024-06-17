;;; ob-datadog.el --- Babel Functions for Datadog metrics    -*- lexical-binding: t; -*-

;; Author: Pavel Popov
;;
;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'ob)

(defun org-babel-expand-body:datadog (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  body)

(append '((datadog . t)) org-babel-load-languages)

(add-to-list 'org-src-lang-modes '("datadog" . text))

(provide 'ob-datadog)

;;; ob-datadog.el ends here
