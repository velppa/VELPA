;;; ob-edn.el --- Babel Functions for EDN format    -*- lexical-binding: t; -*-

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

;; Support for evaluating EDN format

;;; Code:
(require 'ob)

(defun org-babel-expand-body:edn (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  body)

(append '((edn . t)) org-babel-load-languages)

(add-to-list 'org-src-lang-modes '("edn" . clojure))

(provide 'ob-edn)

;;; ob-edn.el ends here
