;;; accord.el --- Xdotool Driven Discord Interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Nicholas Vollmer

;; Author: Nicholas Vollmer <progfolio@protonmail.com>
;; URL: https://github.com/progfolio/accord
;; Created: October 09, 2020
;; Keywords: convenience
;; Package-Requires: ((emacs "27.1") (markdown-mode "0.0.0"))
;; Version: 0.0.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provides an xdotool driven interface to the Discord desktop application.

;;; Code:
(unless (executable-find "xdotool")
  (user-error "Accord cannot run without xdotool. Is it installed and on your PATH?"))

(eval-when-compile (require 'subr-x))
(require 'markdown-mode)

;;; Custom Options

(defgroup accord nil
  "xdotool Driven Discord Interface"
  :group 'convenience
  :prefix "accord-")

(defcustom accord-window-regexp "(#|@)?.*Discord"
  "Regexp to identify Discord window."
  :type 'string)

(defcustom accord-buffer-name "*accord*"
  "Name of the accord buffer."
  :type 'string)

(defcustom accord-process-buffer-name "*accord-process*"
  "Name of the accord process buffer."
  :type 'string)

(defcustom accord-key-delay-time 100
  "Number of milliseconds to delay between each xdotool key press."
  :type 'number)

;;; Variables

(defvar accord--edit-in-progress nil
  "Internal variable for tracking edit progress.")

;;; Functions
(defun accord--current-window ()
  "Return ID for currently focused window."
  (string-trim (shell-command-to-string "xdotool getwindowfocus")))

(defun accord--window-by-name (&optional regexp visible target)
  "Return window ID for window name matching REGEXP.
If VISIBLE is non-nil only search for visible windows.
TARGET used to refocus original window."
  ;;This is more complicated that I would like it to be, but so is life.
  ;;Discord returns several window ID's when searching by name.
  ;;In order to get the one we're interested in sending keys to, we need to
  ;;first focus the window and then redo the search with "--onlyvisible".
  (let* ((current (or target (accord--current-window)))
         (command (concat "xdotool search --name "
                          (when visible "--onlyvisible ")
                          "\"" (or regexp accord-window-regexp) "\""))
         (ids (split-string (shell-command-to-string command) "\n" 'omit-nulls)))
    (if (= (length ids) 1)
        (progn
          (call-process "xdotool" nil (get-buffer-create accord-process-buffer-name)
                        t "windowactivate" current)
          (car ids))
      (dolist (id ids (accord--window-by-name regexp 'visible current))
        (call-process "xdotool" nil (get-buffer-create accord-process-buffer-name)
                      t "windowactivate" id)))))

(defun accord--erase-buffer ()
  "Erase `accord-buffer-name'."
  (with-current-buffer (get-buffer-create accord-buffer-name) (erase-buffer)))

(defun accord-send-commands (&rest commands)
  "Send COMMANDS to target window."
  (let ((current (accord--current-window))
        (target (accord--window-by-name)))
    (apply #'call-process
           `("xdotool" nil ,(get-buffer-create accord-process-buffer-name) t
             "windowactivate" "--sync" ,target
             ,@(mapcar (lambda (el) (format "%s" el)) (flatten-list commands))
             "windowactivate" ,current))))

(defun accord--reset-header-line ()
  "Reset the header-line."
  (with-current-buffer (get-buffer-create accord-buffer-name)
    (setq header-line-format
          (substitute-command-keys
           (concat "\\<accord-mode-map>Accord buffer. "
                   "Send: `\\[accord-send-message]' "
                   "Edit: `\\[accord-edit-message]' "
                   "Delete: `\\[accord-delete-message]'")))))

(defun accord--clear-input ()
  "Return command string to clear input area."
  `("key" "--delay" ,(/ accord-key-delay-time 5) "ctrl+a"
    "keyup" "ctrl+a"
    ;; Using two BackSpaces here because of the way Discord handles quoted text.
    ;; It requires a second delete to clear the inserted quote bar...
    "key" "--delay" ,accord-key-delay-time "BackSpace"
    "key" ,accord-key-delay-time "BackSpace"))

(defun accord--confirm ()
  "Return command string to confirm text input."
  `("key" "--delay" ,accord-key-delay-time "Return"))

(defun accord--copy-input ()
  "Return command string to paste clipboard."
  ;;keyup necessary here?
  `("key" "--delay" ,accord-key-delay-time "ctrl+a" "ctrl+c"))

(defun accord--open-last ()
  "Return command string to open last message."
  ;;keyup necessary here?
  `(,@(accord--clear-input)
    "key" "--delay" ,accord-key-delay-time "Up"))

(defun accord--paste ()
  "Return command string to paste clipboard."
  ;;keyup necessary here?
  `("key" "--delay" ,(/ accord-key-delay-time 4)  "ctrl+v" "keyup" "--delay"
    ,(/ accord-key-delay-time 4) "ctrl+v"))

(defun accord--last-message ()
  "Return last sent message."
  ;; Clear in case we have something already stored in the clipboard
  (gui-set-selection 'CLIPBOARD nil)
  (accord-send-commands
   (accord--open-last)
   (accord--copy-input)
   "Escape")
  (when-let ((selection (gui-get-selection 'CLIPBOARD)))
    (substring-no-properties selection)))

;;; Commands

;;;###autoload
(defun accord-delete-message (&optional noconfirm)
  "Delete last posted message.
If NOCONFIRM is non-nil, do not prompt user for confirmation."
  (interactive "P")
  (if accord--edit-in-progress
      (accord--edit-abort)
    (let (last)
      (unless noconfirm
        (setq last (or (accord--last-message) (user-error "Unable to delete last message"))))
      (when (or noconfirm (yes-or-no-p (format "Delete message?: %S" last)))
        (accord-send-commands
         (accord--open-last)
         (accord--clear-input)
         ;; Discord doesn't let you delete without confirming and the pop-up takes
         ;; some time to appear...
         (let ((accord-key-delay-time (* accord-key-delay-time 3))) (accord--confirm))
         (accord--confirm))))))

(defun accord--edit-abort ()
  "Abort the current edit."
  (setq accord--edit-in-progress nil)
  (accord--erase-buffer)
  (accord--reset-header-line))

;;;###autoload
(defun accord-edit-message ()
  "Edit last message."
  (interactive)
  (unless (derived-mode-p 'accord-mode) (accord))
  (when accord--edit-in-progress
    (user-error "Edit already in progress. Send or Abort."))
  (insert (accord--last-message))
  (goto-char (point-min))
  (setq header-line-format
        (substitute-command-keys
         (concat "\\<accord-mode-map>Accord buffer. "
                 "Send: `\\[accord-send-message]' "
                 "Abort Edit: `\\[accord-delete-message]'"))
        accord--edit-in-progress t))

;;@TODO: send-in-chunks option to bypass charcter limit?
;;;###autoload
(defun accord-send-message ()
  "Send string between START and END as message to Discord.
If region is active, use `mark' and `point' as START and END."
  (interactive)
  (let (keep)
    (when (and accord--edit-in-progress
               (not (string= (buffer-name) accord-buffer-name)))
      (if (setq keep (not
                      (yes-or-no-p
                       (format "Abort edit in progress in %S buffer?"
                               accord-buffer-name))))
          (accord)
        (accord--edit-abort)
        (accord-send-message)))
    (unless keep
      (let ((message (string-trim (apply #'buffer-substring-no-properties
                                         (if (region-active-p)
                                             (list (region-beginning) (region-end))
                                           (list (point-min) (point-max)))))))
        (when (string-empty-p message) (user-error "Can't send empty message"))
        (gui-set-selection 'CLIPBOARD message)
        (accord-send-commands
         (when accord--edit-in-progress (accord--open-last))
         (accord--clear-input)
         (accord--paste)
         (accord--confirm))
        (undo-boundary)
        (accord--erase-buffer)
        (when accord--edit-in-progress
          (accord--reset-header-line)
          (setq accord--edit-in-progress nil))))))

;;;###autoload
(defun accord-channel-last ()
  "Select last selected channel."
  (interactive)
  (accord-send-commands "key" "--delay" (/ accord-key-delay-time 4) "ctrl+k" "Return"))

;;;###autoload
(defun accord-channel-goto-unread ()
  "Goto unread first unread message in channel."
  (interactive)
  (accord-send-commands "key" "--delay" (/ accord-key-delay-time 4) "Shift+Page_Up"))

;;;###autoload
(defun accord-channel-mark-read ()
  "Mark channel as read."
  (interactive)
  (accord-send-commands "key" "--delay" (/ accord-key-delay-time 4) "Escape"))

(defun accord--select (entity direction)
  "Choose previous ENTITY in DIRECTION.
ENTITY may be either `server` or `channel`."
  (interactive)
  (accord-send-commands "key" "--delay" accord-key-delay-time
                        (concat (when (eq entity 'server) "ctrl+") "alt+" direction)))

;;@TODO: should probably take a numeric arg to repeat
;;;###autoload
(defun accord-channel-next ()
  "Select next channel in server."
  (interactive)
  (accord--select 'channel "Down"))

;;;###autoload
(defun accord-channel-prev ()
  "Select previous channel in server."
  (interactive)
  (accord--select 'channel "Up"))

(defun accord-channel--scroll (direction)
  "Scroll channel in DIRECTION."
  (interactive)
  (accord-send-commands "key" "--delay" (/ accord-key-delay-time 4) direction))

;;;###autoload
(defun accord-channel-scroll-down (&optional bottom)
  "Scroll channel down.
If BOTTOM is non-nil, return to bottom of channel."
  (interactive "P")
  (accord-channel--scroll (concat (when bottom "Shift+") "Page_Down")))

;;;###autoload
(defun accord-channel-scroll-up ()
  "Select next channel in server."
  (interactive)
  (accord-channel--scroll "Page_Up"))

;;;###autoload
(defun accord-channel-search (&optional query)
  "Select channel by QUERY."
  (interactive)
  (let ((search (or query (read-string "accord channel: "))))
    (accord-send-commands
     "key" "--delay" "80" "ctrl+k"
     "key" "--delay" "20"
     (cdr (mapcar (lambda (char) (replace-regexp-in-string " " "space" char))
                  (split-string search "")))
     (accord--confirm))))

;;;###autoload
(defun accord-server-prev ()
  "Select previous channel in server."
  (interactive)
  (accord--select 'server "Up"))

;;;###autoload
(defun accord-server-next ()
  "Select next channel in server."
  (interactive)
  (accord--select 'server "Down"))

;;;###autoload
(defun accord ()
  "Toggle accord buffer."
  (interactive)
  (if (string= (buffer-name) accord-buffer-name)
      (delete-window)
    (select-window
     (display-buffer-in-side-window (get-buffer-create accord-buffer-name) '((side . bottom))))
    (unless (derived-mode-p 'accord-mode) (accord-mode))))

;;; Mode definition
(defvar accord-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'accord-send-message)
    (define-key map (kbd "C-c C-e") 'accord-edit-message)
    (define-key map (kbd "C-c C-k") 'accord-delete-message)
    map))

(define-derived-mode accord-mode markdown-mode "accord"
  "Send messages to Discord from Emacs."
  (accord--reset-header-line))

(provide 'accord)

;;; accord.el ends here
