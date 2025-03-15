;;; Code
(require 'js)

(defcustom gron-default "gron | grep "
  "Arguments to `gron'.")

(defcustom gron-buffer-name "*gron*"
  "Buffer for `gron' results.")

(defvar gron-history (list gron-default))

(defun gron (command-args)
  "Run Gron with user-specified COMMAND-ARGS.
The output for the command goes to the `gron-buffer-name' buffer, default \"*gron*\".

Results are written into `gron-buffer-name' buffer."
  (interactive
   (list (read-string "Gron command: "
                      (if current-prefix-arg nil gron-default)
                      'gron-history
                      (if current-prefix-arg nil gron-default))))
  (let* ((regionp (region-active-p))
         (beg (if regionp (region-beginning) (point-min)))
         (end (if regionp (region-end) (point-max)))
         (inhibit-message t))
    (shell-command-on-region beg end command-args gron-buffer-name)
    (display-buffer gron-buffer-name)))

;; (define-key js-mode-map (kbd "C-c g") #'gron)

;; (defun org-babel-execute:restclient (body params)
;;   "Execute a block of Restclient code with org-babel.
;; This function is called by `org-babel-execute-src-block'"
;;   (message "executing Restclient source code block")
;;   (with-temp-buffer
;;     (let ((results-buffer (current-buffer))
;;           (restclient-same-buffer-response t)
;;           (restclient-response-body-only (org-babel-restclient--should-hide-headers-p params))
;;           (restclient-same-buffer-response-name (buffer-name))
;;           (display-buffer-alist
;;            (cons
;;             '("\\*temp\\*" display-buffer-no-window (allow-no-window . t))
;;             display-buffer-alist)))
;;       (insert (buffer-name))
;;       (with-temp-buffer
;;         (dolist (p params)
;;           (let ((key (car p))
;;                 (value (cdr p)))
;;             (when (eql key :var)
;;               (insert (format ":%s = <<\n%s\n#\n" (car value) (cdr value))))))
;;         (insert body)
;;         (goto-char (point-min))
;;         (delete-trailing-whitespace)
;;         (goto-char (point-min))
;;         (restclient-http-parse-current-and-do
;;          'restclient-http-do (org-babel-restclient--raw-payload-p params) t))
;;       (while restclient-within-call
;;         (sleep-for 0.05))
;;       (goto-char (point-min))
;;       (when (equal (buffer-name) (buffer-string))
;;         (error "Restclient encountered an error"))
;;       (when-let* ((jq-header (assoc :jq params))
;;                   (jq-path "jq")
;; 		  (jq-args (or (cdr (assoc :jq-args params)) "")))
;;         (shell-command-on-region
;;          (point-min)
;;          (point-max)
;;          (format "%s %s--args %s" org-babel-restclient--jq-path
;; 		 (if (assq :jq-args params) (format "%s " jq-args) "")
;;                  (shell-quote-argument (cdr jq-header)))
;;          (current-buffer)
;;          t))
;;       (when-let* ((gron-header (assoc :gron params)))
;;         (shell-command-on-region
;;          (point-min)
;;          (point-max)
;;          (format "gron %s" (or  (cdr gron-header) ""))
;;          (current-buffer)
;;          t))
;;       ;; widen if jq but not pure payload
;;       (when (and (assq :jq params)
;;                  (not (assq :noheaders params))
;;                  (not (org-babel-restclient--return-pure-payload-result-p params)))
;;         (widen))

;;       (when (not (org-babel-restclient--return-pure-payload-result-p params))
;;         (org-babel-restclient--wrap-result))

;;       (buffer-string))))

(provide 'gron)
;;; gron.el ends here
