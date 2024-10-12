(defun flag-to-keyword (s)
  "Converts string S containing flag, like -n or --namespace to keyword :n or :namespace."
  (if (string-prefix-p ":" s)
      (intern s)
    (intern (format ":%s" (string-trim-left s "[-]+")))))

;;

(comment
 (flag-to-keyword "-n")
 ;; :n
 (flag-to-keyword "--namespace")
 ;; :namespace
 (flag-to-keyword ":f")
 ;; :f

 (string-trim-left (format "%s" :foo) ":")
 ;; ":foo"
 ;; "foo"

 (length "foo")
 ;; 3
 )


(defun keyword-to-flag (kw)
  (let ((s (string-trim-left (format "%s" kw) ":")))
    (if (eq (length s) 1) (format "-%s" s) (format "--%s" s))))

(comment
 (keyword-to-flag :foo)
 ;; "--foo"
 (keyword-to-flag :n)
 ;; "-n"
 )

(defun sqlc--string-to-list (s)
  "Convert a string S to list."
  (let (tgt
        (lst (string-split s " ")))
    (while lst
      (let ((key (flag-to-keyword (pop lst)))
            (value (string-trim (pop lst) "\"" "\"")))
        (setq tgt (append tgt (cons key `(,value))))))
    tgt))

(comment
 (sqlc--string-to-list "-f foo -b bar --param Key1=Value1 --param Key2=Value2")
 ;; (:f "foo" :b "bar" :param "Key1=Value1" :param "Key2=Value2")
 ;; (:f "foo" :b "bar")
 ;; (:f "foo" :b "bar")
 (append nil '(:f . "foo"))
 ;; (:f . "foo")
 (append nil (cons :f "foo"))
 ;; (:f . "foo")
 (sqlc--string-to-list "-f \"foo\" -b \"bar\"")
 ;; (:f "foo" :b "bar")
 )

(defun sqlc--list-to-string (lst)
  (let (tgt)
    (while lst
      (let ((key (keyword-to-flag (pop lst)))
            (value (format "'%s'" (pop lst))))
        (setq tgt (append tgt `(,key ,value)))))
    (string-join tgt " ")))

(defun sqlc--list-to-args (lst)
  (let (tgt)
    (while lst
      (let ((key (keyword-to-flag (pop lst)))
            (value (pop lst)))
        (setq tgt (append tgt `(,key ,value)))))
    tgt))

(comment
 (sqlc--list-to-string '(:n "redpanda" :foo "bar" :param "k1=v1" :param "k2=\"foo\""))
 ;; "-n 'redpanda' --foo 'bar' --param 'k1=v1' --param 'k2=\"foo\"'"
 ;; "-n 'redpanda' --foo 'bar' --param 'k1=v1'"
 ;; ("-n" "redpanda" "--foo" "bar")
 (sqlc--list-to-string
  (list
   :params (json-encode `((Params . ((RoleSuffix . "prod"))) (OutputPrefix . "foo") (LastLoadedAt . "2024-10-10")))
   :f "foo"))
 ;; "--params '{\"Params\":{\"RoleSuffix\":\"prod\"},\"OutputPrefix\":\"foo\",\"LastLoadedAt\":\"2024-10-10\"}' -f 'foo'"
 ;;  "--params '{\"Params\":{\"RoleSuffix\":\"prod\"},\"OutputPrefix\":\"foo\",\"LastLoadedAt\":\"2024-10-10\"}' -f 'foo'"
 ;; "--Params '{\"Params\":{\"RoleSuffix\":\"prod\"},\"OutputPrefix\":\"foo\",\"LastLoadedAt\":\"2024-10-10\"}'"
 (sqlc--list-to-args
  (list
   :params (json-encode `((Params . ((RoleSuffix . "prod"))) (OutputPrefix . "foo") (LastLoadedAt . "2024-10-10")))
   :f "foo"))
 ;; ("--params" "{\"Params\":{\"RoleSuffix\":\"prod\"},\"OutputPrefix\":\"foo\",\"LastLoadedAt\":\"2024-10-10\"}" "-f" "foo")

 )


(defun sqlc (&rest args)
  "Run sqlc command with provided ARGS."
  (interactive "sArguments for sqlc: ")
  (let* ((args (if (and (eq (length args) 1) (stringp (car args)))
                   (sqlc--string-to-list (car args))
                 args))
         (out-buffer-name "*sqlc*")
         ;; (err-buffer-name "*sqlc-errors*")
         (out-buffer (get-buffer-create out-buffer-name))
         ;; (err-buffer (get-buffer-create err-buffer-name))
         (lines (apply #'process-lines "sqlc" (sqlc--list-to-args args))))
    (message "sqlc args: %s" args)
    (with-current-buffer out-buffer
      (erase-buffer)
      (apply #'insert (mapcar (lambda (l) (concat l "\n")) lines)))
    (switch-to-buffer out-buffer)
    ;; (with-current-buffer err-buffer (erase-buffer))
    ))


(comment
 (process-lines
  "sqlc"
  "-f" "file:///Users/pavel/Developer/src/github.com/FindHotel/search-data-pipelines/sql/search/command/algolia_copy.sql"))

;; (defun my-test-args (&rest args)
;;   (interactive "sArguments: ")
;;   (let ((args (if (and (eq (length args) 1) (stringp (car args)))
;;                   (sqlc--string-to-list (car args))
;;                 args) ))
;;     (message "args: %s" args)
;;     (message "parsed args: %s" (plist-get args :f))))


(provide 'sqlc)
