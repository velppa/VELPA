;;; restutils.el --- My extensions of restclient.el -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Pavel Popov

;; Author: Pavel Popov
;; Keywords: restclient
;; Version: 0.01

(require 'restclient)
(require 'plz)
(require 'plz-see)

(defun restutils-url-to-restclient-buffer (url)
  "Opens a buffer with URL represented as restclient.el GET block."
  (let* ((parsed (url-generic-parse-url (string-trim url)))
         (scheme (url-type parsed))
         (host (url-host parsed))
         (query (url-parse-query-string (cadr (split-string (url-filename parsed) "?"))))
         (symbol-query (mapcar (lambda (kv-pair)
                                 (let ((k (car kv-pair))
                                       (v (cadr kv-pair)))
                                   (list (make-symbol k) v))) query))
         (path (car (split-string (url-filename parsed) "?"))))
    (let ((current-buffer (current-buffer))
          (temp-buffer (switch-to-buffer (format "*restclient-%s*" (make-temp-name "")))))
      (end-of-line)
      (insert (prin1-to-string `(let* ((params ',symbol-query))
                                  (url-build-query-string params))))
      (push-mark)
      (backward-list)
      (lispy-multiline)
      (forward-list)
      (insert "\n#\n")
      (insert (format "GET :host%s?:params\n" path))
      (beginning-of-buffer)
      (insert ":params := <<\n")
      (beginning-of-buffer)
      (insert (format ":host = %s://%s\n" scheme host))
      (restclient-mode))))


(defun restutils-url-to-plz-buffer (url)
  "Opens a buffer with URL represented as plz.el GET block."
  (let* ((parts (split-string url (rx "?") t))
         (query (url-parse-query-string (cadr parts)))
         (symbol-query (mapcar
                        (lambda (kv-pair)
                          (let ((k (car kv-pair)) (v (cadr kv-pair)))
                            (list (make-symbol k) v)))
                        query)))
    (switch-to-buffer (format "*plz-%s*" (make-temp-name "")))
    (end-of-line)
    (insert (prin1-to-string
             `(let* ((params ',symbol-query))
                (plz-see 'get (concat ,(car parts) "?" (url-build-query-string params))))))
    (push-mark)
    (backward-list)
    (lispy-multiline)
    (forward-list)
    (emacs-lisp-mode)))


(defun restutils-copy-curl-command ()
  "Formats the request as a curl command and copies the command to the clipboard."
  (interactive)
  (restclient-http-parse-current-and-do
   '(lambda (method url headers entity)
      (let ((header-args
             (apply 'append
                    (mapcar (lambda (header)
                              (list "-H" (format "'%s: %s'" (car header) (cdr header))))
                            headers))))
        (kill-new (concat "curl "
                          (mapconcat 'identity
                                     (append '("-i")
                                             (list (concat "-X" method))
                                             (list (format "'%s'" url))
                                             header-args
                                             (when (> (string-width entity) 0)
                                               (list "-d" entity)))
                                     " "))))
      (message "curl command copied to clipboard."))))

(provide 'restutils)
;;; restutils.el ends here
