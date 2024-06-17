;; tufte-html.el --- Summary;
;;; URL https://gitlab.com/-/snippets/22309, https://jnboehm.gitlab.io/blog/tufte-css/

;;; Commentary:

;; (setq org-html-footnote-format "<label for=\"%s\" class=\"margin-toggle sidenote-number\"></label> <input type=\"checkbox\" id=\"1\" class=\"margin-toggle\"/>")

(require 'ox)

;;; Code:
(setq org-html-footnotes-section "<!-- %s --><!-- %s -->")

(org-export-define-derived-backend 'tufte-html 'html
  :translate-alist '((footnote-reference . tufte-html-footnote-reference)
		     (footnote-definition . tufte-html-footnote-definition) ; not useful
		     (link . tufte-html-margin-note-link)
		     (src-block . tufte-html-src-block)))

(defun tufte-html-footnote-reference (footnote-reference contents info)
  "Create a footnote according to the tufte css format.
FOOTNOTE-REFERENCE is the org element, CONTENTS is nil.  INFO is
a plist holding contextual information."
  (format "<label for=\"%s\" class=\"margin-toggle sidenote-number\"></label><input type=\"checkbox\" id=\"%s\" class=\"margin-toggle\"/><span class=\"sidenote\">%s</span>"
	  (org-export-get-footnote-number footnote-reference info)
	  (org-export-get-footnote-number footnote-reference info)
	  (org-trim (org-export-data (org-export-get-footnote-definition footnote-reference info) info))))

(defun tufte-html-margin-note-link (link desc info)
  "LINK is the margin note (or not).

If it is not, it willl be passed onto the original function in
order to be handled properly.  DESC is the description part of
the link.  INFO is a plist holding contextual information."
  (let ((path (split-string (org-element-property :path link) ":")))
    (if (and (string= (org-element-property :type link) "fuzzy")
	     (string= (car path) "mn"))
	(format "<label for=\"%s\" class=\"margin-toggle\">&#8853;</label><input type=\"checkbox\" id=\"%s\" class=\"margin-toggle\"/> <span class=\"marginnote\">%s</span>"
		(cadr path) (cadr path)
		desc)
    (org-html-link link desc info))))

;; This function has no use.
;; (defun tufte-html-footnote-definition (footnote-definition contents info)
;;   "Create a footnote according to the tufte css format.  FOOTNOTE-DEFINITION is the org element, CONTENTS is nil.  INFO is a plist holding contextual information."
;;   "")

(defun tufte-html-src-block (src-block contents info)
  "Transcode an SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (format "<pre class=\"code\">%s</pre>"
	  (org-html-format-code src-block info)))


;; This function was used to verify my output.  It's pretty similar
;; (read copied) from the beamer export.
(defun org-tufte-html-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a Myhtml buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between the bo

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org MYHTML Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'tufte-html "*Org MYHTML Export*"
    async subtreep visible-only body-only ext-plist (lambda () (html-mode))))


(provide 'tufte-html)
;;; tufte-html.el ends here
