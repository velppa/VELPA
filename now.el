;;; now.el --- Keep track of Now pages -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Pavel Popov

;; Author: Pavel Popov
;; Keywords: org-mode
;; Version: 0.01

(require 'rx)
(require 'org-ql)
(require 'org-roam)
(require 'org)
(require 'hyperbole)
(require 'dash)

(defun now--goto-node-by-id (id)
  "Go to the Org-roam node with the specified ID."
  (let ((node (org-roam-node-from-id id)))
    (if node
        (org-roam-node-visit node t t)
      (message "Node with ID '%s' not found." id))))

(defun now--create-hyp-ebut (label id)
  (let (btn)
    (hattr:set 'btn 'lbl-key label)
    (hattr:set 'btn 'loc (file-name-nondirectory (buffer-file-name)))
    (hattr:set 'btn 'dir default-directory)
    ;; (hattr:set 'btn 'actype nil)
    (hattr:set 'btn 'actype 'link-to-org-id)
    ;; (hattr:set btn 'action 'now--goto-node-by-id)
    (hattr:set 'btn 'args `(,id))
    (ebut:create 'btn)
    ))

(defun now--shortcut-story-short-format (org-link)
  "Format Org Link for Shortcut Story"
  (let ((expr (rx "[[" (group (one-or-more anything) "/story/"
                              (group (one-or-more digit)) "/"
                              (one-or-more anything))
                  "]" (one-or-more anything) "]")))
    (when (string-match expr org-link)
      (let* ((url (match-string 1 org-link))
             (id (match-string 2 org-link))
             (title (format "sc-%s" id)))
        (format "[[%s][%s]]" url title)))))

(defun now-format-url (url)
  "Format url as org-mode link."
  (when url
    ))

(defun now--shortcut-story-ebut (org-link org-id)
  "Returns hyperbole explicit button + Org Mode link"
  (let ((expr (rx "[[" (group (one-or-more anything) "/story/"
                              (group (one-or-more digit)) "/"
                              (one-or-more anything))
                  "]" (one-or-more anything) "]")))
    (when (string-match expr org-link)
      (let* ((url (match-string 1 org-link))
             (story-id (match-string 2 org-link))
             (label (format "sc-%s" story-id)))
        (when (now--create-hyp-ebut label org-id)
          (format "<(%s)> [[%s][link]]" label url))))))

(comment
 (now--shortcut-story-short-format "[[https://app.shortcut.com/findhotel/story/133422/consume-raw-clicks-in-price-accuracy-heater][Consume raw clicks in Price Accuracy Heater | Shortcut]]")
 ;; "[[https://app.shortcut.com/findhotel/story/133422/consume-raw-clicks-in-price-accuracy-heater][sc-133422]]"
 ;; "133422"

 (when (and tags (string-match-p (regexp-quote ":Story:") tags) url)
          (now--shortcut-story-short-format url))
 )

(cl-defun now-roadmap-item (&key (todo t) (priority t) (closed t))
  "Formats heading as a Roadmap item."
  (let* ((props (org-entry-properties))
         (title (org-get-heading t (not todo) (not priority) t))
         (url (now--format-url (cdr (assoc-string "URL" props))))
         (tags (cdr (assoc-string "TAGS" props)))
         (closed-date (when closed (-some-->
                                       (org-entry-get nil "CLOSED")
                                     (substring it 1 15)
                                     (format "[%s]" it)))))
    (string-join `(,title ,url ,closed-date) " ")))

(cl-defun now-hyperbolized-item (&key (todo t))
  "Formats heading with Explicit Hyperbole button."
  ;; (org-get-heading t t t t)
  ;; (org-heading-components)
  ;; (message (file-name-nondirectory (buffer-file-name)))
  (let* ((props (org-entry-properties))
         (title (org-get-heading t (not todo) nil t))
         (url (cdr (assoc-string "URL" props)))
         (id (cdr (assoc-string "ID" props)))
         (tags (cdr (assoc-string "TAGS" props))))
    (string-join
     (list
      title
      (when (and tags (string-match-p (regexp-quote ":Story:") tags) url)
        ;; (now--shortcut-story-short-format url)
        (if id
            (now--shortcut-story-ebut url id)
          title)))
     " ")))

(cl-defun now-sort-by (&key (by 'closed))
  "Return a function to sort strings by BY criteria."
  (lambda (s1 s2)
    (let* ((expr (rx "[" (group (repeat 4 (any "0-9")) "-"
                                (repeat 2 (any "0-9")) "-"
                                (repeat 2 (any "0-9")) " "
                                (repeat 3 (any "A-Za-z")))
                     "]" eol))
           (date1 (when (string-match expr s1) (match-string 1 s1)))
           (date2 (when (string-match expr s2) (match-string 1 s2))))
      (if (and date1 date2)
          (progn
            (message "matched")
            (string-greaterp date1 date2))
        (progn
          (message "not matched")
          (string-lessp s1 s2))))))


(provide 'now)
