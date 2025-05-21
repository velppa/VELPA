;;; my-org-publish.el --- My functions publish Org Roam projects -*- lexical-binding: t; -*-

;;; COMMENTARY
;; This code requires org-hugo-base-dir local variable to be configured.

;;; CODE

;; (require 'cl)
(require 'pcre2el)
(require 'pp-html)
(require 'templatel)

;; (defun org-export-set-default-html-options (&optional _)
;;   "Set default HTML export options for exporting Org Mode files."
;;   (message "Setting default org-export-html options.")
;;   (setq org-export-with-toc nil
;;         org-export-with-section-numbers nil
;;         org-html-preamble nil
;;         org-html-postamble 'auto
;;         org-html-head "
;; <link rel=\"stylesheet\" type=\"text/css\" href=\"https://gongzhitaao.org/orgcss/org.css\"/>
;; "))

;; (org-export-set-default-html-options)

(defun my-org-publish--rg (args)
  (apply #'process-lines-ignore-status "rg" args))

(defun my-org-publish-files (filetag &optional dir)
  "Return a list of filenames in DIR to publish that have FILETAG file tag."
  (unless dir
    (setq dir default-directory))
  (let* ((pattern (thread-last
                    `(seq bol "#+filetags:" (* nonl) ":" ,filetag ":" (* nonl) eol)
                    rx-to-string
                    rxt-elisp-to-pcre))
         (default-directory dir))
    (my-org-publish--rg `("--glob" "*.org" "--files-with-matches" ,pattern))))


(defun my-org-publish-assets (filetag linktype dir)
  "Return a list of assets to publish.

Starts from DIR, looking for files and attachements with FILETAG
with given LINKTYPE."
  (let ((posts (my-org-publish-files filetag dir))
        (default-directory dir)
        (pattern (thread-first
                   `(: "[" ,linktype ":" (group (* (not "]"))))
                   rx-to-string
                   rxt-elisp-to-pcre)))
    (my-org-publish--rg
     (cl-concatenate
      'list
      `("--no-line-number" "--no-filename" "--only-matching" "--replace" "$1" ,pattern)
      posts))))

(defvar my-org-publish-notes-dir "~/Notes")
(defvar my-org-publish-repos-dir "~/Developer/src/github.com/microsoft")
(defvar my-org-publish-repos '("vscode" "TypeScript"))

(defun my-org-publish-wiki (repo)
  "Publish Github wiki for REPO, a directory relative to `my-org-publish-repos-dir'."
  (interactive
   (list (completing-read "Repository: " my-org-publish-repos)))
  (let*
      ((base-dir my-org-publish-notes-dir)
       (repos-dir my-org-publish-repos-dir)
       (note-preparation-function
        (lambda (_) (message "note-preparation-function called.")))
       (note-completion-function
        (lambda (props)
          (message "note-completion-function called with %s" props)
          (let* ((default-directory (plist-get props :publishing-directory))
                 (base-directory (plist-get props :base-directory))
                 (include (thread-last
                            (plist-get props :include)
                            (mapcar (lambda (org-file) (string-replace ".org" "" org-file)))))
                 (links-expr (thread-first
                               `(: "(" (group ,(cons 'or include)) ".md)")
                               (rx-to-string t)
                               rxt-elisp-to-pcre))
                 (ids-expr (thread-first
                             `(: "[" (group (+? any)) "](" (+? any) ".md)")
                             (rx-to-string t)
                             rxt-elisp-to-pcre)))
            ;; (message "expr: %s" links-expr)
            (shell-command (format "perl -p -i -e 's/%s/(\\1)/g' *.md" links-expr))
            (shell-command (format "perl -p -i -e 's/%s/\\1/g' *.md" ids-expr))
            (shell-command (format "perl -p -i -e 's|%s/||g' *.md" base-directory))))))
    (let ((org-publish-project-alist
           `((,(concat repo ":notes")
              :base-directory ,base-dir
              :publishing-directory ,(file-name-concat repos-dir repo)
              :exclude-tags ("noexport")
              :include ,(my-org-publish-files repo base-dir)
              :base-extension "nonexistent"
              :publishing-function org-gfm-publish-to-gfm
              :preparation-function ,note-preparation-function
              :completion-function ,note-completion-function
              :auto-sitemap t
              :recursive t
              :sitemap-sort-files chronologically
              ;; :sitemap-title "Wiki"
              :sitemap-filename "Home.md")
             (,(concat repo ":attachments")
              :base-directory ,(file-name-concat base-dir "assets")
              :publishing-directory ,(file-name-concat repos-dir repo "assets")
              :include ,(my-org-publish-assets repo "attachment" base-dir)
              :base-extension "nonexistent"
              :publishing-function org-publish-attachment)
             (,repo
              :components (,(concat repo ":notes") ,(concat repo ":attachments"))))))
      (org-publish repo t))))

(defun my-org-publish-blog-navigation ()
  '(div (a :href "/" "posts")
        " - "
        (a :href "/reading_list.html" "reading list")
        ;; " - "
        ;; (a :href "/subscribe.html" "subscribe")
        ))

(defun my-org-publish-blog-html-preamble (&optional _)
  (pp-html `(div .masthead
                 ((div .site-title
                       (h1 .stitle "Scratchpad"))
                  ,(my-org-publish-blog-navigation)
                  ))))

(defun my-org-publish-blog-html-postamble ()
  `(("en" ,(pp-html `(div
                    (div .post-meta "Last updated on: %C")
                    (footer (p ,(my-org-publish-blog-navigation))
                            (p "Build with %c")))))))

(defvar my-org-publish-blog-files
  (my-org-publish-files "Blog" my-org-publish-notes-dir))

(defvar my-org-publish-a
  (thread-first
    `(: "<a href=\""
        (group (*? anything) (zero-or-one ".html"))
        (zero-or-one "#" (* any)) "\">"
        (group (*? any))
        "</a>")
    (rx-to-string t)))

(defun my-org-publish-links-filter (text backend info)
  "Keep only links to exported files."
  (when (org-export-derived-backend-p backend 'html)
    (string-match my-org-publish-a text)
    (let* ((url (substring text (match-beginning 1) (match-end 1)))
           (parsed (url-generic-parse-url url))
           (schema (url-type parsed))
           (file (url-filename parsed))
           (label (substring text (match-beginning 2) (match-end 2))))
      (if (or schema
              (not file)
              (equal file "")
              (cl-find-if
               (lambda (x)
                 (equal x (file-name-with-extension file ".org")))
               my-org-publish-blog-files))
          text
        ;; (message "text: %s" text)
        (pp-html `(span ,label))))))

;; (add-to-list 'org-export-filter-link-functions
;;              #'my-org-publish-links-filter)

(defun my-org-publish-blog (&optional force)
  "Publish my blog."
  (interactive)
  (setq my-org-publish-blog-files (my-org-publish-files filetag base-dir))
  (let*
      ((base-dir "~/Documents/Notes")
       (filetag "Blog"))
    (let ((org-publish-project-alist
           `(("blog:notes"
              :base-directory ,base-dir
              :publishing-directory ,org-hugo-base-dir
              :exclude-tags ("noexport")
              :include ,my-org-publish-blog-files
              :base-extension "nonexistent"
              :publishing-function org-html-publish-to-html
              ;; :preparation-function ,note-preparation-function
              ;; :completion-function ,note-completion-function
              :auto-sitemap t
              :recursive t
              :sitemap-sort-files chronologically
              :sitemap-title " "
              :sitemap-filename "index.html")
             ("blog:attachments"
              :base-directory ,(file-name-concat base-dir "assets")
              :publishing-directory ,(file-name-concat org-hugo-base-dir "assets")
              :include ,(my-org-publish-assets filetag "attachment" base-dir)
              :base-extension "nonexistent"
              :publishing-function org-publish-attachment)
             ("blog:files"
              :base-directory ,base-dir
              :publishing-directory ,org-hugo-base-dir
              :include ,(my-org-publish-assets filetag "file" base-dir)
              :base-extension "nonexistent"
              :publishing-function org-publish-attachment)
             ("blog"
              :components ("blog:notes" "blog:attachments" "blog:files"))))
          (org-export-with-toc nil)
          (org-export-with-section-numbers nil)
          (org-html-head-include-default-style nil)
          (org-html-head "
<link rel=\"stylesheet\" type=\"text/css\" href=\"/static/simple-v1.css\"/>
<link rel=\"stylesheet\" type=\"text/css\" href=\"/static/madeup.css\"/>
<script src=\"https://cdn.jsdelivr.net/npm/darkmode-js@1.5.7/lib/darkmode-js.min.js\"></script>
<script>
  const options = {label: '🌓'};
  window.addEventListener('load', function() { new Darkmode(options).showWidget(); });
</script>
"
                         )
          (org-html-preamble #'my-org-publish-blog-html-preamble)
          (org-export-filter-link-functions '(my-org-publish-links-filter))
          (org-html-postamble t)
          (org-html-postamble-format (my-org-publish-blog-html-postamble)))
      (org-publish "blog" force))
    (format "Blog published to %s directory at %s."
            org-hugo-base-dir
            (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))))


(defun my-org-publish-hugo (&optional force)
  "Publish my Digital Garden website with ox-hugo."
  (interactive)
  (setq my-org-publish-blog-files (my-org-publish-files filetag base-dir))
  (let*
      ((base-dir my-org-publish-notes-dir)
       (filetag "Blog")
       (org-hugo-default-section-directory ""))
    (let ((org-publish-project-alist
           `(("blog:notes"
              :base-directory ,base-dir
              :publishing-directory ,org-hugo-base-dir
              :exclude-tags ("noexport")
              :include ,my-org-publish-blog-files
              :base-extension "nonexistent"
              :publishing-function org-hugo-export-to-md
              ;; :preparation-function ,note-preparation-function
              ;; :completion-function ,note-completion-function
              :auto-sitemap t
              :recursive t
              :sitemap-sort-files chronologically
              :sitemap-title " "
              :sitemap-filename "index.html")
             ("blog:attachments"
              :base-directory ,(file-name-concat base-dir "assets")
              :publishing-directory ,(file-name-concat org-hugo-base-dir "assets")
              :include ,(my-org-publish-assets filetag "attachment" base-dir)
              :base-extension "nonexistent"
              :publishing-function org-publish-attachment)
             ("blog:files"
              :base-directory ,base-dir
              :publishing-directory ,org-hugo-base-dir
              :include ,(my-org-publish-assets filetag "file" base-dir)
              :base-extension "nonexistent"
              :publishing-function org-publish-attachment)
             ("blog"
              :components ("blog:notes" "blog:attachments" "blog:files"))))
          (org-export-with-toc nil)
          (org-export-with-section-numbers nil)
          (org-html-head-include-default-style nil)
          (org-html-head "
<link rel=\"stylesheet\" type=\"text/css\" href=\"/static/simple-v1.css\"/>
<link rel=\"stylesheet\" type=\"text/css\" href=\"/static/madeup.css\"/>
<script src=\"https://cdn.jsdelivr.net/npm/darkmode-js@1.5.7/lib/darkmode-js.min.js\"></script>
<script>
  const options = {label: '🌓'};
  window.addEventListener('load', function() { new Darkmode(options).showWidget(); });
</script>
"
                         )
          (org-html-preamble #'my-org-publish-blog-html-preamble)
          (org-export-filter-link-functions '(my-org-publish-links-filter))
          (org-html-postamble t)
          (org-html-postamble-format (my-org-publish-blog-html-postamble)))
      (org-publish "blog" force))
    (format "Blog published to %s directory at %s."
            org-hugo-base-dir
            (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))))

(defun my-org-publish-cleanup (dir)
  "Clean up published notes, assets from DIR."
  (let* (;; (dir "~/Developer/src/github.com/velppa/velppa.github.io")
         (default-directory dir))
    (when (directory-files dir nil "assets")
      (delete-directory (file-name-concat dir "assets") t t))
    (thread-last
      (directory-files dir)
      (cl-remove-if-not (lambda (filename) (equal (file-name-extension filename t) ".html")))
      (cl-mapcar #'delete-file))))


(defun my-org-publish-body-filter (body backend info)
  "Transforming the body."
  (when (org-export-derived-backend-p backend 'html)
    (templatel-render-file
     (file-name-concat org-hugo-base-dir "templates" "default.html.jinja")
     `(("notoc" . t)
       ("title" . "Some title")
       ("body" . ,body)))))

(defun my-org-publish--parse-org-keyword (keyword)
  "Parse a single Org-Mode document KEYWORD."
  (let ((key (org-element-property :key keyword))
        (value (org-element-property :value keyword)))
    (cons key value)))

(defun my-org-publish--publish-file (file)
   (let*
       (kwrds
        out-file
        (body-filter
         (lambda (body backend info)
           (when (org-export-derived-backend-p backend 'html)
             (setq my-temp-kwrds kwrds)
             (let ((title  (alist-get "TITLE" kwrds nil nil #'equal)))
               (templatel-render-file
                (file-name-concat org-hugo-base-dir "templates" "default.html.jinja")
                `(("notoc" . t)
                  ("title" . ,title)
                  ("body" . ,body)))))))
        (org-html-head-include-default-style nil)
        (org-html-head "")
        (org-export-filter-body-functions `(,body-filter))
        (org-export-filter-link-functions '(my-org-publish-links-filter))
        (org-html-html5-fancy t)
        (org-html-head-extra "")
        (org-html-head-include-scripts nil)
        (org-html-preamble nil)
        (org-html-postamble nil)
        (org-html-use-infojs nil)
        (keyword-advice
         (lambda (keyword _c _i)
           (when-let ((kw (my-org-publish--parse-org-keyword keyword)))
             (setq kwrds (cons kw kwrds)))))
        (template-advice (lambda (contents _i) contents)))
     (with-current-buffer (find-file-noselect file)
       (advice-add 'org-html-template :override template-advice)
       (advice-add 'org-html-keyword :before keyword-advice)
       (setq out-file (org-html-export-to-html))
       (advice-remove 'org-html-keyword keyword-advice)
       (advice-remove 'org-html-template template-advice))
     (rename-file
      (file-name-concat (file-name-directory file) out-file)
      org-hugo-base-dir t)
     out-file))


(provide 'my-org-publish)
;;; my-org-publish.el ends here
