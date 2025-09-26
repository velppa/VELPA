;;; now.el --- Keep track of Now pages -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Pavel Popov

;; Author: Pavel Popov
;; Keywords: org-mode
;; Version: 0.01

(require 'rx)
(require 'org-ql)
(require 'org-roam)
(require 'org)
;; (require 'hyperbole)
(require 'dash)


(defvar now-priority-category-alist
  '(("example" . "https://www.notion.so/foobar/example-120f584cf1ae48ae8cdacc73fe1b4928"))
  "Mapping from PRIORITY_CATEGORY property to URL linked to it.")

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

(defconst now--org-link-rx
  (rx "[[" (group (one-or-more (not "]"))) "]"
      (optional "[" (group (one-or-more anything)) "]") "]")
  "Regexp to extract description and url from Org Mode link.")


(defun now--format-org-link (url &optional title)
  "Format LINK using TITLE if provided, replacing original title."
  (when (string-match now--org-link-rx url)
    (let ((raw-url (match-string 1 url))
          (existing-title (match-string 2 url)))
      (format "[[%s][%s]]" raw-url (or title existing-title)))))


(defun now--format-url (url)
  "Format URL as org-mode link."
  (let ((shortcut-story-rx (rx "[[" (group "https://app.shortcut.com/"
                                           (one-or-more anything)
                                           "/" (group (or "story" "epic" "objective")) "/"
                                           (group (one-or-more digit))
                                           (optional (one-or-more (not "]"))))
                               "]" (optional (one-or-more anything)) "]")))
    (when url
      (or (when (string-match shortcut-story-rx url)
            (let* ((raw-url (match-string 1 url))
                   (kind (match-string 2 url))
                   (id (match-string 3 url))
                   ;; (title (format "sc-%s" id))
                   )
              (format "[[%s][%s]]" raw-url kind)))
          (now--format-org-link url "link")
          (format "[[%s][link]]" url)))))


(comment
 (now--format-url "[[https://app.shortcut.com/findhotel/story/133422/consume-raw-clicks-in-price-accuracy-heater][Consume raw clicks in Price Accuracy Heater | Shortcut]]")
 (now--format-url "[[https://app.shortcut.com/findhotel/epic/100758][Price Graph | Epics | Shortcut]]")

 ;; "[[https://app.shortcut.com/findhotel/story/133422/consume-raw-clicks-in-price-accuracy-heater][sc-133422]]"
 ;; "133422"

 (when (and tags (string-match-p (regexp-quote ":Story:") tags) url)
   (now--shortcut-story-short-format url))
 )

(cl-defun now-roadmap-item (&key (todo t) (priority t) (closed t) (category t))
  "Formats heading as a Roadmap item.

When CATEGORY is t, add MILESTONE_CATEGORY property to the title,
when link, make it a link by looking up in `now-priority-category-alist'."
  (let* ((props (org-entry-properties))
         (title (org-get-heading t (not todo) (not priority) t))
         (url (now--format-url (cdr (assoc-string "URL" props))))
         (category (when category
                     (-some--> (assoc-string "MILESTONE_CATEGORY" props)
                       cdr
                       (if-let ((priority-url
                                 (and (equal category 'link)
                                      (assoc it now-priority-category-alist))))
                           (format "| [[%s][%s]] |" (cdr priority-url) it)
                         (format "| %s |" it)))))
         ;; (tags (cdr (assoc-string "TAGS" props)))
         (closed-date (-some--> closed
                        identity
                        (org-entry-get nil "CLOSED")
                        (substring it 1 15)
                        (format "[%s]" it))))
    (string-clean-whitespace (string-join (remove nil `(,title ,category ,url ,closed-date)) " "))))

(comment
 (setq now-items-props '()))


(cl-defun now-milestone (&key (include-priority t) (include-closed t) (dummy t))
  "Formats heading as a Roadmap item.

When CATEGORY is t, add MILESTONE_CATEGORY property to the title,
when link, make it a link by looking up in `now-priority-category-alist'."
  (let* ((props (org-entry-properties))
         (title (org-get-heading t t t t))
         (todo-state (cdr (assoc "TODO" props)))
         (priority (cdr (assoc "PRIORITY" props)))
         (owner (-some--> (assoc "MILESTONE_OWNER" props) cdr))
         (roadmap-item (-some--> (assoc "ROADMAP_ITEM" props) cdr (now--format-org-link it "roadmap item")))
         (url (now--format-url (cdr (assoc-string "URL" props))))
         (category (-some--> (assoc-string "MILESTONE_CATEGORY" props)
                     cdr
                     (format "%s" it)))
         ;; (tags (cdr (assoc-string "TAGS" props)))
         (closed-date (and include-closed
                           (-some-->
                               (assoc "CLOSED" props)
                             cdr
                             (substring it 1 15)
                             (format "[%s]" it)))))
    ;; (add-to-list 'now-items-props props)
    (-->
     `(,todo-state
       ,(and include-priority priority)
       ,title
       ,(or category "")
       ,(or owner "")
       ,(or roadmap-item "")
       ,(or url "")
       ,closed-date)
     (remove nil it)
     (string-join it " | ")
     string-clean-whitespace
     (format "%s|\n" it))))

(comment
 (assoc "Comprehensive Hotel Content" now-priority-category-alist))

;; (cl-defun now-hyperbolized-item (&key (todo t))
;;   "Formats heading with Explicit Hyperbole button."
;;   ;; (org-get-heading t t t t)
;;   ;; (org-heading-components)
;;   ;; (message (file-name-nondirectory (buffer-file-name)))
;;   (let* ((props (org-entry-properties))
;;          (title (org-get-heading t (not todo) nil t))
;;          (url (cdr (assoc-string "URL" props)))
;;          (id (cdr (assoc-string "ID" props)))
;;          (tags (cdr (assoc-string "TAGS" props))))
;;     (string-join
;;      (list
;;       title
;;       (when (and tags (string-match-p (regexp-quote ":Story:") tags) url)
;;         ;; (now--shortcut-story-short-format url)
;;         (if id
;;             (now--shortcut-story-ebut url id)
;;           title)))
;;      " ")))

(cl-defun now-sort-by (&key (by 'closed))
  "Return a function to sort strings BY criteria."
  (lambda (s1 s2)
    (let* ((expr (rx "[" (group (repeat 4 (any "0-9")) "-"
                                (repeat 2 (any "0-9")) "-"
                                (repeat 2 (any "0-9")) " "
                                (repeat 3 (any "A-Za-z")))
                     "]"))
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
