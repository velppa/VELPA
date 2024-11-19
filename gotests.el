;;; gotests.el --- Emacs package for https://github.com/cweill/gotests  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Damien Levin
;; Copyright (C) 2024  Pavel Popov

;; Author: Damien Levin
;; Maintainer: Pavel Popov
;; Keywords: go gotests languages
;; Version: 0.1.0
;; URL: https://github.com/velppa/VELPA/blob/main/gotests.el

;; Apache License (version 2.0).

;;; Code:

(defun gotests--ensure-not-in-test ()
  "Check if we are in a test file."
  (when (string-match (rx "_test.go" eol) buffer-file-name)
    (user-error "Cannot generate gotests from test file.")))

(defun gotests()
  "Generate all missing go tests."
  (interactive)
  (gotests--ensure-not-in-test)
  (call-process "gotests" nil nil nil "-all" "-w" buffer-file-name)
  (message "Generated all of test codes."))

(defun gotests-function ()
  "Generate test for function at point."
  (interactive)
  (gotests--ensure-not-in-test)
  (let* ((node (treesit-node-at (point)))
         (func-node (treesit-parent-until
                     node
                     (lambda (x) (equal (treesit-node-type x) "function_declaration"))))
         (func-name (treesit-node-text (car (treesit-node-children func-node t)))))
    (call-process
     "gotests" nil nil nil "-w" "-only" func-name buffer-file-name)
    (message "Generated test code for func %s. See *_test.go file." func-name)))

(provide 'gotests)
;;; gotests.el ends here
