;;; vega-view-save.el --- Vega visualization saver      -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Pavel Popov

;; Author: Pavel Popov <pavelpopov@me.com>
;; Created: 2022-11-01
;; Version: 1.0
;; Keywords: multimedia
;; Package-Requires: ((emacs "25") (cider "0.24.0") (parseedn "0.1"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; vega-view-save is an Emacs Lisp library for saving Vega
;; specifications.

;;; Code:

(require 'vega-view)
(require 'ob)

(defun vega-view-save--json (json-string vega-buffer out-file)
  "Passes `JSON-STRING` through the Vega command line tools,
saving results to OUT-FILE."
  (cl-assert (or (image-type-available-p 'svg) (image-type-available-p 'png))
             nil
             "vega-view requires an emacs that supports either SVG or PNG!")
  (with-current-buffer vega-buffer
    (fundamental-mode)
    (setq buffer-read-only nil) ; cider likes to set results buffers read-only
    (erase-buffer)
    (insert json-string)
    ;; Only switch the output buffer to image-mode if the command was
    ;; successful so any error text will be visible in the buffer.
    (let ((coding-system-for-read 'raw-text) ; in case it's a PNG
          (vega-view-command (if (and (image-type-available-p 'svg)
                                      (not vega-view-prefer-png))
                                 `(,vega-view--vega-svg-command
                                   ,(string-join `("-h -b "
                                                   ,(or vega-view-base-directory default-directory))))
                               `(,vega-view--vega-png-command ""))))
      (call-process-region (point-min)
                           (point-max)
                           (car vega-view-command)
                           t `(:file ,out-file) nil
                           (cadr vega-view-command)))
    ;; (display-buffer vega-buffer)
    ))

(defun vega-view-save--clojure (body vega-buffer out-file)
  "Evaluate `CLOJURE-FORM-STRING` in the cider context of the
buffer from which it is called, convert the result to JSON, then
pass it to `vega-view-save--json' to save definition in
`VEGA-BUFFER` and save in `OUT-FILE'."
  (cl-assert (member 'cider-mode minor-mode-list)
             nil
             "view-view requires an active cider connection for use with clojure forms!")

  (let ((lexical-binding t))
    (with-current-buffer vega-buffer
      (setq cider-popup-output-marker (point-marker)))
    (cider-interactive-eval
     ;; in case local printer settings would truncate the output
     (format "(do (set! *print-length* nil) %s)" body)
     (nrepl-make-response-handler
      vega-buffer
      (lambda (buffer value)
        (vega-view-save--json (json-encode (parseedn-read-str value)) buffer out-file))
      (lambda (_buffer out)
        (cider-emit-interactive-eval-output out))
      (lambda (_buffer err)
        (cider-emit-interactive-eval-err-output err))
      nil
      nil
      nil
      (lambda (buffer warning)
        (cider-emit-into-popup-buffer buffer warning 'font-lock-warning-face t))))))

(defvar org-babel-default-header-args:vega-clojure
  '((:results . "file") (:exports . "results"))
  "Default arguments to use when evaluating a vega-clojure source block.")

(defun org-babel-execute:vega-clojure (body params)
  "Compiles returned Clojure document using `vega-lite'"
  (let* ((out-file (cdr (or (assq :file params)
			    (error "You need to specify a :file parameter"))))
         (filename (org-babel-process-file-name out-file))
         (buffer (get-buffer-create "*vega*")))
    (vega-view-save--clojure body buffer filename)
    nil))

(defun vega-view-save--elisp (body params buffer filename)
  "Evaluate `BODY` then pass result to `vega-view-save--json' to save definition in
`BUFFER` and save in `FILENAME'."
  (let* ((lexical (cdr (assq :lexical params)))
         (result-params (cdr (assq :result-params params)))
         (body (format (if (member "output" result-params)
                           "(with-output-to-string %s\n)"
                         "(progn %s\n)")
                       (org-babel-expand-body:emacs-lisp body params)))
         (result (eval (read body) (org-babel-emacs-lisp-lexical lexical))))
    (vega-view-save--json result buffer filename)))

(defvar org-babel-default-header-args:vega-json
  '((:results . "file") (:exports . "results"))
  "Default arguments to use when evaluating a vega-json source block.")

(defun org-babel-execute:vega-json (body params)
  "Compiles returned JSON document using `vega-lite'"
  (let* ((out-file (cdr (or (assq :file params)
			    (error "You need to specify a :file parameter"))))
         (filename (org-babel-process-file-name out-file))
         (buffer (get-buffer-create "*vega*")))
    (vega-view-save--elisp body params buffer filename)
    nil))

(provide 'vega-view-save)
;;; vega-view-save.el ends here
