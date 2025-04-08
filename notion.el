;;; notion.el --- Integration with Notion API  -*- lexical-binding: t; -*-


(defun notion-rich-text (key value)
  `(,key (type . rich_text) (rich_text . [((type . text) (text (content . ,value)))])))

(defun notion-title (key value)
  `(,key (type . title) (title . [((type . text) (text (content . ,value)))])))

(defun notion-select (key value)
  `(,key (type . select) (select (name . ,value))))

(defun notion-multi-select (key &rest values)
  `(,key (type . multi_select)
         (multi_select . ,(mapcar (lambda (value) `((name . ,value))) values))))
