;;; aerospace.el --- Utilities to work with AeroSpace  -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025 Pavel Popov

;; Author: Pavel Popov <hotter-plazas-0x@icloud.com>
;; URL: https://github.com/velppa/velpa/tree/main/aerospace.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.2"))
;; Keywords: convenience

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

;; This package provides utilities to work with AeroSpace.
;;; Code:

(require 'tomelr)

(defvar aerospace-config-file-path
  "~/.config/aerospace/aerospace.toml"
  "Path to AeroSpace config file.")

(defvar aerospace-gap 0 "Gap between windows.")

(defvar aerospace-root-config
  `(;;
    (start-at-login . t)
    (enable-normalization-flatten-containers . t)
    (enable-normalization-opposite-orientation-for-nested-containers . t)
    (accordion-padding . 30)
    (default-root-container-layout . "tiles")
    (default-root-container-orientation . "auto")
    (key-mapping.preset . "qwerty")
    (on-focused-monitor-changed . ("move-mouse monitor-lazy-center")))
  "Root AeroSpace configuration.")


(defvar aerospace-modes-config
  `((mode.main.binding
     . ,(append '((cmd-shift-7 . "layout h_tiles")
                  (cmd-shift-6 . "layout h_accordion")
                  (cmd-shift-9 . "focus left")
                  (cmd-shift-0 . "focus right")
                  ;;
                  (alt-tab . "workspace-back-and-forth")
                  (alt-shift-tab . "move-workspace-to-monitor --wrap-around next")
                  ;;
                  ;; (cmd-left . "focus left")
                  ;; (cmd-down . "focus down")
                  ;; (cmd-up . "focus up")
                  ;; (cmd-right . "focus right")
                  ;;
                  (f9 . "mode keys")
                  (f13 . "mode keys")
                  ;; (f14 . "move-workspace-to-monitor --wrap-around next")
                  )
                (mapcar (lambda (x)
                          (cons (make-symbol (format "alt-%s" x))
                                (format "workspace %s" x)))
                        (number-sequence 0 9))))
    (mode.keys.binding
     . ,(append '((esc . "mode main")
                  (f9 . "mode main")
                  (f13 . "mode main")
                  ;; (g . "mode main")
                  ;; (tab . "mode main")
                  ;;
                  (period . "layout tiles horizontal vertical")
                  (comma . "layout accordion horizontal vertical")
                  ;;
                  (m . "mode move")
                  ;;
                  (h . "focus left")
                  (j . "focus down")
                  (k . "focus up")
                  (l . "focus right")
                  ;;
                  (minus . "resize smart -50")
                  (equal . "resize smart +50")
                  ;;
                  (g . "flatten-workspace-tree") ;; mnemonics - C-g => escape
                  ;; (r . "reload-config")
                  (f . "layout floating tiling")
                  (backspace . "close-all-windows-but-current")
                  ;; workspaces on Planck
                  (q . ("workspace 1" "mode main"))
                  (w . ("workspace 2" "mode main"))
                  (e . ("workspace 3" "mode main"))
                  (r . ("workspace 4" "mode main"))
                  (t . ("workspace 5" "mode main"))
                  (y . ("workspace 6" "mode main"))
                  (u . ("workspace 7" "mode main"))
                  (i . ("workspace 8" "mode main"))
                  (o . ("workspace 9" "mode main"))
                  (p . ("workspace 0" "mode main"))
                  ;;
                  (left . ("join-with left" "mode main"))
                  (down . ("join-with down" "mode main"))
                  (up . ("join-with up" "mode main"))
                  (right . ("join-with right" "mode main")))
                (mapcar (lambda (x)
                          (cons (make-symbol (format "%s" x))
                                (format "workspace %s" x)))
                        (number-sequence 0 9))))
    (mode.move.binding
     . ,(append '((h . ("move left" "mode main"))
                  (j . ("move down" "mode main"))
                  (k . ("move up" "mode main"))
                  (l . ("move right" "mode main"))
                  (m . ("move-workspace-to-monitor --wrap-around next" "mode main")))
                (mapcar (lambda (x)
                          (cons (make-symbol (format "%s" x))
                                (list (format "move-node-to-workspace %s" x) "mode main")))
                        (number-sequence 0 9)))))
  "AeroSpace mode bindings configuration.")


(defvar aerospace-callbacks-config
  `((on-window-detected . (((if.app-id . "org.gnu.Emacs")
                            (run . ("move-node-to-workspace 2")))
                           ;; ((if.app-id . "com.apple.Safari")
                           ;;  (run . ("move-node-to-workspace 3")))
                           ((if.app-id . "org.yanex.marta")
                            (run . ("move-node-to-workspace 6")))
                           ((if.app-id . "com.mitchellh.ghostty")
                            (run . ("move-node-to-workspace 0"))))))
  "AeroSpace callbacks configuration.")


(defun aerospace-gap-block (size)
  "Return uniform gap configuration block of SIZE."
  (let ((ratio (* 1.5 (/ 16.0 9.0))))
    `(gaps
      . ((inner.horizontal . ,size)
         (inner.vertical . ,size)
         (outer.left . ,(format "%d" (* ratio size)))
         (outer.bottom . ,size)
         (outer.top . ,size)
         (outer.right . ,(format "%d" (* ratio size)))))))


(comment
 (aerospace-gap 10)
 ;; (gaps (inner.horizontal . 10) (inner.vertical . 10) (outer.left . 10) (outer.bottom . 10) (outer.top . 10) (outer.right . 10))

 (aerospace-set-gap 200)

 (aerospace-gap-block 200)
 ;; (gaps (inner.horizontal . 200) (inner.vertical . 200) (outer.left . "355") (outer.bottom . 200) (outer.top . 200) (outer.right . "355"))
 )
;;

;;;; Commands

(defun aerospace-encode-config ()
  (tomelr-encode
   (append
    aerospace-root-config
    aerospace-modes-config
    aerospace-callbacks-config
    (list (aerospace-gap-block aerospace-gap)))))

;;;###autoload
(defun aerospace-apply ()
  "Write current AeroSpace configuration into a file and reload-config."
  (interactive)
  (with-temp-file aerospace-config-file-path
    (insert (aerospace-encode-config)))
  (start-process "aerospace" nil "aerospace" "reload-config")
  (message "AeroSpace config reloaded."))

;;;###autoload
(defun aerospace-set-gap (n)
  "Set gap between windows in AeroSpace to N pixels."
  (interactive "nGap in pixels: ")
  (setq aerospace-gap n)
  (aerospace-apply))

;;;; Footer

(provide 'aerospace)

;;; aerospace.el ends here
