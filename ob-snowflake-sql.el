;;; ob-snowflake-sql.el --- Run Snowflake query via CIDER with Clojure

;; Copyright (C) 2023 Pavel Popov

;; Author: Pavel Popov
;; Keywords: snowflake, clojure, sider
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Org-Babel support for evaluating go code.
;;
;; Much of this is modeled after `ob-C'. Just like the `ob-C', you can specify
;; :flags headers when compiling with the "go run" command. Unlike `ob-C', you
;; can also specify :args which can be a list of arguments to pass to the
;; binary. If you quote the value passed into the list, it will use `ob-ref'
;; to find the reference data.
;;
;; If you do not include a main function or a package name, `ob-go' will
;; provide it for you and it's the only way to properly use
;;
;; very limited implementation:
;; - currently only support :results output.
;; - not much in the way of error feedback.
;; - cannot handle table or list input.

;;; Requirements:
;; - CIDER

;;; Code:
(require 'cider)
(require 'tempo)

(comment (setq params '()))

(defun ob-clojure-eval-with-cider (expanded params)
  "Evaluate EXPANDED code block with PARAMS using cider."
  (let ((connection (cider-current-connection (cdr (assq :target params))))
	(result-params (cdr (assq :result-params params)))
	result0)
    (unless connection (sesman-start-session 'CIDER))
    (if (not connection)
	;; Display in the result instead of using `user-error'
	(setq result0 "Please reevaluate when nREPL is connected")
      (ob-clojure-with-temp-expanded expanded params
	(let ((response (nrepl-sync-request:eval exp connection)))
	  (push (or (nrepl-dict-get response "err") ;; <-- added this condition
                    (nrepl-dict-get response "root-ex")
		    (nrepl-dict-get response "ex")
		    (nrepl-dict-get
		     response (if (or (member "output" result-params)
				      (member "pp" result-params))
				  "out"
				"value")))
		result0)))
      (unless (car result0) (setq result0 '("No data found")))
      (ob-clojure-string-or-list
       (reverse (delete "" (mapcar (lambda (r)
				     (replace-regexp-in-string "nil" "" r))
				   result0)))))))

(comment
 (let ((body "select 1"))
   (thread-last
     `((require '[velppa.snowflake :as sf])
       (sf/execute-print! [,body]))
     prin1-to-string
     (string-remove-suffix ")")
     (string-remove-prefix "(")))
 ;; "(require '[velppa.snowflake :as sf]) (sf/execute-print! [\"select 1\"])"
 )


(defun org-babel-execute:snowflake-sql (body params)
  "Execute the SQL statements in BODY with PARAMS in Snowflake using Clojure."
  (message "Executing \"%s\"" body)
  (let ((role (or (cdr (assq :role params)) "SQD_SEARCH")))
    (ob-clojure-eval-with-cider
     (thread-last
       ;; first item is a block of Clojure code
       `(do
            (require '[velppa.snowflake :as sf])
            (sf/execute-print! [,body] :role ,role))
       prin1-to-string)
     params)))

(append '((snowflake-sql . t)) org-babel-load-languages)
(add-to-list 'org-src-lang-modes '("snowflake-sql" . sql))


(let ((key "<csql")
      (name "snowflake-sql"))
  (tempo-define-template
   name
   '("#+begin_src snowflake-sql :results output :eval no-export :exports both" n
     p n
     "#+end_src")
   key
   (format "Insert a %s block" name)))


(provide 'ob-snowflake-sql)
;;; ob-snowflake-sql.el ends here
