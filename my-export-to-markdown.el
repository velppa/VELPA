;;; my-export-to-markdown.el --- My custom exporter from Org Mode to Markdown -*- lexical-binding: t; -*-

(require 'org)
(require 'f)
(require 'ox-gfm)

;;; Code

(defun my-export-subtree-to-markdown-location ()
  "Export element at point to markdown into the file specified in LOCATION property."
  (interactive)
  (let ((org-md-link (lambda (link contents info) (format "[%s](%s)" contents path)))
        (org-export-with-tags nil)
        (org-export-show-temporary-export-buffer nil))
    (when-let ((location (format "%s" (org-property-or-variable-value 'LOCATION)))
               (title (format "%s" (or (org-entry-get nil "TITLE")
                                       (org-get-heading t t t t)))))
      (let ((org-md-toplevel-hlevel (if (equal title "\"\"") 1 2)))
        ;; (message "title   : %s hlevel: %s" title org-md-toplevel-hlevel)
        (when (equal title "\"\"") (message "empty"))
        (org-gfm-export-as-markdown nil t))
      (with-current-buffer "*Org GFM Export*"
        (goto-char (point-min))
        (unless (equal title "\"\"")
          (insert (format "# %s\n\n" title)))
        (write-region (point-min) (point-max) location)
        (message "File %s written" location)))))


(defun my-export-to-markdown ()
  "Export all headings that have LOCATION property to markdown."
  (interactive)
  (save-excursion
    (org-map-entries #'my-export-subtree-to-markdown-location "LOCATION<>\"\"")))


(defun my-org-md-link (orig-fn &rest args)
  "Copies files to assets next to export_file_name."
  (let* ((link (car args))
         (desc (cadr args))
         (path (org-element-property :path link))
         (type (org-element-property :type link))
         (file (f-filename path))
         (export-file-name (-> '("export_file_name") org-collect-keywords car cadr))
         (target-dir (f-join (f-dirname export-file-name) "assets"))
         (target-file (f-join target-dir file))
         (source (f-join "assets" file)))
    (if (equal type "file")
        (progn
          (make-directory target-dir t)
          (copy-file path target-file t)
          (format "![%s](%s)" desc source))
      (apply orig-fn args))))


(comment
 (advice-add 'org-md-link :around #'my-org-md-link)

 (advice-remove 'org-md-link #'my-org-md-link)

 (global-set-key (kbd "C-c v")
                 (lambda ()
                   (interactive)
                   ;; (message "foo")
                   (let ((result (org-map-entries #'org-element-at-point
                                                  "LOCATION<>\"\"")))
                     (message "result: %s" result))
                   ))
 )


(provide 'my-export-to-markdown)
;;; my-export-to-markdown.el ends here
