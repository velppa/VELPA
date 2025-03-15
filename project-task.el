;;; project-task.el --- Project-specific task execution -*- lexical-binding: t -*-

;; Copyright (C) 2025 Pavel Panchekha

;; Author: Pavel Panchekha
;; Keywords: convenience, tools
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Provides consistent task execution commands across different projects
;; using dir-locals.el for project-specific configuration.

;;; Code:

(defvar project-task-run-default-cmd "make run"
  "Default command for running project.")

(defvar project-task-build-default-cmd "make build"
  "Default command for building project.")

(defvar project-task-lint-default-cmd "make lint"
  "Default command for linting project.")

(defun project-task (task-var default-cmd)
  "Execute a project task defined in .dir-locals.el.
TASK-VAR is a symbol matching a variable name in .dir-locals.el.
The variable's value should be a command string that will be passed to `compile'.
If no matching task is found, display a message indicating the missing task."
  (let* ((default-directory (project-root (project-current)))
         (cmd (if (boundp task-var) (symbol-value task-var)
                default-cmd)))
    (compile cmd)))

;;;###autoload
(defun project-task-run ()
  "Run project-specific run command."
  (interactive)
  (project-task 'project-task-run project-task-run-default-cmd))

;;;###autoload
(defun project-task-build ()
  "Run project-specific build command."
  (interactive)
  (project-task 'project-task-build project-task-build-default-cmd))

;;;###autoload
(defun project-task-lint ()
  "Run project-specific lint command."
  (interactive)
  (project-task 'project-task-lint project-task-lint-default-cmd))

(provide 'project-task)
;;; project-task.el ends here
