;;; quartz.el --- Helper functions to publish notes from Org Roam to Quartz. -*- lexical-binding: t; -*-

;;; CODE

(defun quartz--rg (args)
  (message "")
  (apply #'process-lines-ignore-status "rg" args))

(defun quartz-exported-notes (dir)
  "Return a list of filenames in DIR to export to Quartz."
  (let* ((pattern (thread-last
                    `(seq bol "#+quartz:" (* nonl) "true" (* nonl) eol)
                    rx-to-string
                    rxt-elisp-to-pcre))
         (default-directory dir))
    (quartz--rg `("--glob" "*.org" "--files-with-matches" ,pattern))))


(defvar quartz-target-dir "~/Developer/src/github.com/velppa/velppa.github.io"
  "Target directory for Quartz export.")

(defvar quartz-source-directories '("~/Documents/Notes")
  "List of source directories to find notes to publish.")

(defun quartz-export (dir)
  "Export Org files from DIR into Quartz markdown files."
  (interactive
   (list (completing-read "Directory: " quartz-source-directories)))
  (delete-directory (file-name-concat quartz-target-dir "content") t)
  ;; (make-directory (file-name-concat quartz-target-dir "static/"))
  (dolist (f (quartz-exported-notes dir))
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
  (message "Quartz markdown files and assets exported."))

(defun quartz-export-this-file ()
  "Export current Org file into Quartz markdown file."
  (interactive)
  (let ((org-hugo-base-dir quartz-target-dir)
        (org-hugo-default-section-directory "notes")
        (org-export-use-babel nil)
        (org-export-with-tags nil)
        (org-export-with-todo-keywords nil)
        ;; (org-export-with-tasks nil)
        )
    (message "exporting %s to Quartz" (buffer-file-name))
    (org-hugo-export-to-md)))


(defun quartz-build ()
  "Build HTML files from Markdown files using Quartz."
  (interactive)
  (let ((default-directory quartz-target-dir))
    (async-shell-command "make build" "*quartz*"))
  (message "Quartz built."))

(defun quartz-run ()
  "Run Quartz local webserver."
  (interactive)
  (let ((default-directory quartz-target-dir))
    (async-shell-command "make run" "*quartz-server*")))


(comment
 (quartz-exported-notes "~/Documents/Notes")
 (quartz-export "~/Documents/Notes")
 )


(provide 'quartz)
;;; quartz.el ends here
