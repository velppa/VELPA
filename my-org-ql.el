(require 'rx)

(defun my-org-ql--shortcut-story-short-format (org-link)
  (let ((expr (rx "[[" (group (one-or-more anything) "/story/"
                              (group (one-or-more digit)) "/"
                              (one-or-more anything))
                  "]" (one-or-more anything) "]")))
    (when (string-match expr org-link)
      (let* ((url (match-string 1 org-link))
             (id (match-string 2 org-link))
             (title (format "sc-%s" id)))
        (format "[[%s][%s]]" url title)))))

(comment
 (my-org-ql--shortcut-story-short-format "[[https://app.shortcut.com/findhotel/story/133422/consume-raw-clicks-in-price-accuracy-heater][Consume raw clicks in Price Accuracy Heater | Shortcut]]")
 ;; "[[https://app.shortcut.com/findhotel/story/133422/consume-raw-clicks-in-price-accuracy-heater][sc-133422]]"
 ;; "133422"
 )

(cl-defun my-org-ql-roadmap-item (&key (todo t))
  "Formats heading as a Roadmap item."
  ;; (org-get-heading t t t t)
  ;; (org-heading-components)
  (let* ((props (org-entry-properties))
         (title (org-get-heading t (not todo) t t))
         (url (cdr (assoc-string "URL" props)))
         (tags (cdr (assoc-string "TAGS" props))))
    (if (and tags (string-match-p (regexp-quote ":Story:") tags) url)
        (format "%s [%s]" title (my-org-ql--shortcut-story-short-format url))
      (format "%s" title))))

(provide 'my-org-ql)
