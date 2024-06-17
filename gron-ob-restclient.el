(defun org-babel-execute:restclient (body params)
  "Execute a block of Restclient code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing Restclient source code block")
  (with-temp-buffer
    (let ((results-buffer (current-buffer))
          (restclient-same-buffer-response t)
          (restclient-response-body-only (org-babel-restclient--should-hide-headers-p params))
          (restclient-same-buffer-response-name (buffer-name))
          (display-buffer-alist
           (cons
            '("\\*temp\\*" display-buffer-no-window (allow-no-window . t))
            display-buffer-alist)))

      (insert (buffer-name))
      (with-temp-buffer
        (dolist (p params)
          (let ((key (car p))
                (value (cdr p)))
            (when (eql key :var)
              (insert (format ":%s = <<\n%s\n#\n" (car value) (cdr value))))))
        (insert body)
        (goto-char (point-min))
        (delete-trailing-whitespace)
        (goto-char (point-min))
        (restclient-http-parse-current-and-do
         'restclient-http-do (org-babel-restclient--raw-payload-p params) t))

      (while restclient-within-call
        (sleep-for 0.05))

      (goto-char (point-min))
      (when (equal (buffer-name) (buffer-string))
        (error "Restclient encountered an error"))

      (when-let* ((jq-header (assoc :jq params))
		  (jq-args (or (cdr (assoc :jq-args params)) "")))
        (shell-command-on-region
         (point-min)
         (point-max)
         (format "%s %s--args %s" org-babel-restclient--jq-path
		 (if (assq :jq-args params) (format "%s " jq-args) "")
                 (shell-quote-argument (cdr jq-header)))
         (current-buffer)
         t))

      (when-let* ((gron-header (assoc :gron params))
		  (gron-args (or (cdr (assoc :gron-args params)) gron-args)))
        (message "running gron %s" gron-header)
        (shell-command-on-region
         (point-min)
         (point-max)
         (format "%s %s" gron-path (or (cdr gron-header) ""))
         (current-buffer)
         t))

      ;; widen if jq but not pure payload
      (when (and (assq :jq params)
                 (not (assq :noheaders params))
                 (not (org-babel-restclient--return-pure-payload-result-p params)))
        (widen))

      (when (not (org-babel-restclient--return-pure-payload-result-p params))
        (org-babel-restclient--wrap-result))

      (buffer-string))))
