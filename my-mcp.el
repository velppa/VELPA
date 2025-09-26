;;; my-mcp.el --- Helpers for MCP servers -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pavel Popov

;; Author: Pavel Popov <hotter-plazas-0x@icloud.com>
;; Version: 0.1-pre
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience, mcp, llm, ai

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

;;;; Installation

;;;;; Manual

;; Install these required packages:

;; + https://github.com/karthink/gptel
;; + https://github.com/lizqwerscott/mcp.el

;; Then put this file in your load-path, and put this in your init
;; file:

;; (require 'my-mcp)

;;; Code:

;;;; Requirements
(require 'mcp)
(require 'gptel)

(defun mcp-make-tools ()
  "Make gptel tools from mcp tools."
  (interactive)
  (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
    (mapc (lambda (tool) (apply #'gptel-make-tool tool)) tools)
    (message "%s tools made" (length tools))))


(defun mcp-register-tools ()
  "Register mcp tools for use with gptel."
  (interactive)
  (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
    (mapcar (lambda (tool)
              (let ((path `(,(plist-get tool :category) ,(plist-get tool :name))))
                (push (gptel-get-tool path) gptel-tools)
                (message "registered %s tool" path)))
            tools)))


(defun mcp-deregister-tools ()
  "Remove registration of mcp tools from gptel."
  (interactive)
  (thread-last
    (mcp-hub-get-all-tool :asyncp t :categoryp t)
    (mapcar
     (lambda (tool)
       (setq gptel-tools
             (cl-remove-if
              (lambda (x)
                (and
                 (equal (plist-get tool :category) (gptel-tool-category x))
                 (equal (plist-get tool :name) (gptel-tool-name x))))
              gptel-tools))))))


(defun mcp-make-and-register-tools ()
  "Make gptel tools and then register them."
  (interactive)
  (mcp-deregister-tools)
  (mcp-make-tools)
  (mcp-register-tools)
  (message "mcp-make-and-register-tools finished"))


(defun mcp-get-resource (connection-name resource-uri)
    "Get and display an MCP resource in a new buffer .
CONNECTION-NAME is the name of the MCP server connection.
RESOURCE-URI is the URI of the resource to retrieve."
    (interactive
     (let* ((connection-names (hash-table-keys mcp-server-connections))
            (connection-name (completing-read "Connection: " connection-names nil t))
            (connection (gethash connection-name mcp-server-connections))
            (resources (mcp--resources connection))
            (resource-uris (mapcar (lambda (res) (plist-get res :uri)) resources))
            (resource-uri (completing-read "Resource: " resource-uris nil t)))
       (list connection-name resource-uri)))
    (let* ((connection (gethash connection-name mcp-server-connections))
           (resource-data (mcp-read-resource connection resource-uri))
           (contents (plist-get resource-data :contents))
           (first-content (aref contents 0))
           (text (plist-get first-content :text))
           (buffer-name (format "*mcp-%s*" resource-uri)))
      (with-current-buffer (get-buffer-create buffer-name)
        (erase-buffer)
        (insert text)
        (goto-char (point-min))
        (pop-to-buffer (current-buffer)))))

;;;; Footer
(provide 'my-mcp)
