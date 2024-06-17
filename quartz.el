;;; quartz.el --- Helper functions to publish notes from Org Roam to Quartz. -*- lexical-binding: t; -*-

;;; CODE

(defun quartz--rg (args)
  (message "")
  (apply #'process-lines-ignore-status "rg" args))

(defun quartz-published-notes (dir)
  "Return a list of filenames in DIR to publish to Quartz."
  (let* ((pattern (thread-last
                    `(seq bol "#+publish:" (* nonl) "true" (* nonl) eol)
                    rx-to-string
                    rxt-elisp-to-pcre))
         (default-directory dir))
    (quartz--rg `("--glob" "*.org" "--files-with-matches" ,pattern))))


(defvar quartz-target-dir "~/Developer/src/github.com/velppa/velppa.github.io"
  "Target directory for Quartz export.")


(defun quartz-publish (dir)
  "Publish Quartz files from DIR containing Org Mode files."
  (interactive
   (list (completing-read "Directory: " '("~/Documents/Notes"))))
  (delete-directory (file-name-concat quartz-target-dir "content") t)
  ;; (make-directory (file-name-concat quartz-target-dir "static/"))
  (dolist (f (quartz-published-notes dir))
    (with-current-buffer (find-file-noselect (file-name-concat dir f))
      (let ((org-hugo-base-dir quartz-target-dir)
            (org-hugo-default-section-directory "notes")
            (org-export-use-babel nil)
            (org-export-with-tags nil)
            (org-export-with-todo-keywords nil)
            ;; (org-export-with-tasks nil)
            )
        (message "exporting %s to Quartz" f)
        (org-hugo-export-to-md))))
  (rename-file (file-name-concat quartz-target-dir "static" "assets")
               (file-name-concat quartz-target-dir "content/"))
  (let ((default-directory quartz-target-dir))
    (async-shell-command "make build" "*quartz*"))
  (message "Quartz files and assets published."))


(comment
 (quartz-published-notes "~/Documents/Notes")
 (quartz-publish "~/Documents/Notes")
 )


(provide 'quartz)
;;; quartz.el ends here
