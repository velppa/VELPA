;;; phraxos.el --- Package description (don't include the word "Emacs")  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pavel Popov

;; Author: Pavel Popov <hotter-plazas-0x@icloud.com>
;; URL: https://github.com/velppa/VELPA/main/phraxos.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "30.1"))
;; Keywords: blog

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package allows flanges to be easily frobnicated.

;;;; Installation

;;;;; Manual

;; Install these required packages:

;; + https://github.com/joddie/pcre2el
;; + https://github.com/ox-tufte/ox-tufte

;; Then put this file in your load-path, and put this in your init
;; file:

;; (require 'phraxos)

;;;; Usage

;; Run one of these commands:

;; `phraxos-publish': Publish the blog.

;;;; Tips

;; + You can customize settings in the `phraxos' group.

;;;; Credits

;; This package would not have been possible without the Phraxos[1]
;; static website generator.
;;
;;  [1]  https://phraxos.caffeine.computer/

;;; Code:

;;;; Requirements
(require 'pcre2el)
(require 'ox-tufte)


;;;; Customization

(defgroup phraxos nil
  "Settings for `phraxos'."
  :link '(url-link "https://example.com/phraxos.el"))

(defcustom phraxos-filetag "blog"
  "Filetag of posts to include into publishing."
  :type 'string)

(defcustom phraxos-dir "~/Notes"
  "Directory of Org files."
  :type 'string)

(defcustom phraxos-publishing-dir "/Users/pavel/Developer/src/github.com/velppa/velppa.github.io/out"
  "Directory of published HTML files before processing with phraxos.sh."
  :type 'string)

(defcustom phraxos-assets-dir "/Users/pavel/Developer/src/github.com/velppa/velppa.github.io/out/images"
  "Directory where to publish assets."
  :type 'string)


;;;; Variables

(defvar phraxos-var nil
  "A variable.")

;;;;; Keymaps

;;;; Commands

;;;###autoload
(defun phraxos-publish ()
  "Publish phraxos website, with universal argument, then publish with force.

Publishing process does:
1) build HTMLs from Org Mode files in `phraxos-dir' tagged with `phraxos-filetag'.
2) copy assets
3) builds final HTML using phraxos.sh
"
  (interactive)
  (let ((org-publish-project-alist
         `(("phraxos:posts"
            :base-directory ,phraxos-dir
            :publishing-directory ,phraxos-publishing-dir
            :exclude-tags ("noexport")
            :include ,(phraxos-files-to-publish)
            :base-extension "nonexistent"
            :publishing-function org-tufte-publish-to-html
            ;; :preparation-function ,note-preparation-function
            ;; :completion-function ,note-completion-function
            :auto-sitemap t
            :recursive t
            :sitemap-sort-files chronologically
            :sitemap-title "Just another blog"
            :sitemap-filename "index.html"
            :sitemap-format-entry  (lambda (entry style project)
                                     (cond ((not (directory-name-p entry))
                                            (format "%s [[file:%s][%s]]"
                                                    (format-time-string "%Y-%m-%d"
                                                                        (org-publish-find-date entry project))
                                                    entry
                                                    (org-publish-find-title entry project)))
                                           ((eq style 'tree)
                                            ;; Return only last subdir.
                                            (file-name-nondirectory (directory-file-name entry)))
                                           (t entry)))
            :with-author nil
            :body-only nil
            :html-preamble "<nav>
                              <a href=\"/\">Home</a>
                            </nav>"
            :html-postamble "<footer>
                                <span>Exported: %d</span>
                                <a href=\"#\" style=\"margin-left: 30px;\">top</a>
                            </footer>")
           ("phraxos:attachments"
            :base-directory ,(file-name-concat phraxos-dir "assets")
            :publishing-directory ,phraxos-assets-dir
            :include ,(phraxos-assets-to-publish "attachment")
            :base-extension "nonexistent"
            :publishing-function org-publish-attachment)
           ("phraxos:files"
            :base-directory ,phraxos-dir
            :publishing-directory ,phraxos-assets-dir
            :include ,(phraxos-assets-to-publish "file")
            :base-extension "nonexistent"
            :publishing-function org-publish-attachment)
           ("phraxos"
            :components ("phraxos:posts" "phraxos:attachments" "phraxos:files"))))
        (org-export-with-toc nil)
        (org-export-with-section-numbers nil)
        (org-html-head-include-default-style nil)
        (org-html-head "<link rel=\"stylesheet\" href=\"/assets/tufte.css\" type=\"text/css\" />
                        <link rel=\"stylesheet\" href=\"/assets/ox-tufte.css\" type=\"text/css\" />")
        ;; (org-export-filter-link-functions '(my-org-publish-links-filter))
        ;; (org-html-postamble t)
        ;; (org-html-postamble-format (my-org-publish-blog-html-postamble))
        )
    (org-publish "phraxos" current-prefix-arg)))


;;;; Functions

;;;;; Public

(defun phraxos-files-to-publish ()
  "Return a list of filenames in `phraxos-dir' to publish that have `phraxos-filetag' filetag."
  (let* ((pattern (thread-last
                    `(seq bol "#+filetags:" (* nonl) ":" ,phraxos-filetag ":" (* nonl) eol)
                    rx-to-string
                    rxt-elisp-to-pcre))
         (default-directory phraxos-dir))
    (phraxos--rg `("--glob" "*.org" "--files-with-matches" ,pattern))))


(defun phraxos-assets-to-publish (linktype)
  "Return a list of assets to publish.

Starts from `phraxos-dir', looking for files and attachements with `phraxos-filetag'
with given LINKTYPE."
  (let ((posts (phraxos-files-to-publish))
        (default-directory phraxos-dir)
        (pattern (thread-first
                   `(: "[" ,linktype ":" (group (* (not "]"))))
                   rx-to-string
                   rxt-elisp-to-pcre)))
    (phraxos--rg
     (cl-concatenate
      'list
      `("--no-line-number" "--no-filename" "--only-matching" "--replace" "$1" ,pattern)
      posts))))



;;;;; Private

(defun phraxos--rg (args)
  "Runs ripgrep with supplied ARGS."
  (apply #'process-lines-ignore-status "rg" args))

;;;; Footer

(provide 'phraxos)

;;; phraxos.el ends here
