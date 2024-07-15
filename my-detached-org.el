;;; my-detached-org.el --- Detached integration for org -*- lexical-binding: t -*-

;; Copyright (C) 2024 velppa

;;; Commentary:

;; This package contains my-detached-org-babel-sh that
;; fixes multiline strings in detached-org.el

;;; Code:

;;;; Requirements

(require 'detached)
(require 'ob-shell)

(defun my-detached-org-babel-sh (org-babel-sh-evaluate-fun &rest args)
  "Modify ARGS before calling ORG-BABEL-SH-EVALUATE-FUN.

This function modifies the full-body in ARGS and replaces it with a
`detached' command.  The functionality is enabled by setting a header
property of :detached t in the org babel src block."
  (pcase-let* ((`(,session ,full-body ,params ,stdin ,cmdline) args))
	(if (alist-get :detached params)
		(cl-letf* ((detached-session-origin 'org)
				   (detached-session-action detached-org-session-action)
				   (detached-session-mode 'detached)
				   (new-command ;; (thread-last
                                                ;;   full-body
                                                ;;   (replace-regexp-in-string "\\\\\n" " ")
                                                ;;   (replace-regexp-in-string "\n" " "))
                                    full-body)
                   (detached-session (detached-create-session new-command))
				   (dtach-command
					(if (string= "none" (alist-get :session params))
						(detached-session-start-command detached-session
                                                        :type 'string)
					  (format "%s\necho \"[detached]\"" (detached-session-start-command detached-session
                                                                                        :type 'string))))
				   ((symbol-function #'org-babel-eval)
					(lambda (_ command)
					  (start-file-process-shell-command "detached-org" nil command)
					  "[detached]")))
		  (apply org-babel-sh-evaluate-fun `(,session ,dtach-command ,params ,stdin ,cmdline)))
	  (apply org-babel-sh-evaluate-fun args))))

(advice-add 'detached-org-babel-sh :override #'my-detached-org-babel-sh)

(provide 'my-detached-org)

;;; my-detached-org.el ends here
