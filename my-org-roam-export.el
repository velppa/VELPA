;;; my-org-roam-export.el --- My custom exporter from Org Roam -*- lexical-binding: t; -*-

(require 'ox-md)

(defun org-roam-export--org-md--reference (datum info &optional named-only)
  "Org-roam's patch for `org-md--reference' to support ID link export.
See `org-html--reference' for DATUM, INFO and NAMED-ONLY."
  (let* ((type (org-element-type datum))
         (user-label
          (org-element-property
           (pcase type
             ((or `headline `inlinetask) :CUSTOM_ID)
             ((or `radio-target `target) :value)
             (_ :name))
           datum))
         (user-label
          (or user-label
              (when-let ((path (org-element-property :ID datum)))
                ;; see `org-html-link' for why we use "ID-"
                ;; (search for "ID-" in ox-html.el)
                (concat "ID-" path)))))
    (cond
     ((and user-label
           (or (plist-get info :html-prefer-user-labels)
               (memq type '(headline inlinetask))))
      user-label)
     ((and named-only
           (not (memq type '(headline inlinetask radio-target target)))
           (not user-label))
      nil)
     (t
      (org-export-get-reference datum info)))))

(advice-add 'org-html--reference :override #'my-org-roam-export--org-html--reference)

(provide 'my-org-roam-export)
;;; my-org-roam-export.el ends here
