;;; accord.el --- Lazy Emacs to Discord Interface  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile (require 'subr-x))
(require 'markdown-mode)

;;; Custom Options
(defgroup accord nil
  "Lazy Emacs to Discord Interface"
  :group 'convenience
  :prefix "accord-")

(defcustom accord-window-regexp "(#|@).*Discord"
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

;;; Functions
(defun accord-send-commands (&rest commands)
  "Send COMMANDS to target window."
  (let ((current (accord--current-window))
        (target (accord--window-by-name)))
    (apply #'call-process
           `("xdotool" nil ,(get-buffer-create accord-process-buffer-name) t
             "windowactivate" "--sync" ,target
             ,@(mapcar (lambda (el) (format "%s" el)) (flatten-list commands))
             "windowactivate" ,current))))

(defun accord--current-window ()
  "Return ID for currently focused window."
  (string-trim (shell-command-to-string "xdotool getwindowfocus")))

(defun accord--xdotool-clear-input ()
  "Return command string to clear input area."
  `("key" "--delay" "5" "ctrl+a"
    "key" "--delay" ,accord-key-delay-time "Delete"))

(defun accord--xdotool-open-last ()
  "Return command string to open last message."
  ;;keyup necessary here?
  `(,@(accord--xdotool-clear-input)
    "key" "--delay" ,accord-key-delay-time "Up"))

(defun accord--xdotool-paste ()
  "Return command string to paste clipboard."
  ;;keyup necessary here?
  `("key" "--delay" ,(/ accord-key-delay-time 4)  "ctrl+v" "keyup" "--delay" "20" "ctrl+v"))

(defun accord--xdotool-copy-input ()
  "Return command string to paste clipboard."
  ;;keyup necessary here?
  `("key" "--delay" ,accord-key-delay-time "ctrl+a" "ctrl+c"))

(defun accord--xdotool-confirm ()
  "Return command string to confirm text input."
  `("key" "--delay" ,accord-key-delay-time "Return"))

(defun accord--window-by-name (&optional regexp)
  "Return window ID for window name matching REGEXP."
  (string-trim (shell-command-to-string
                (concat "xdotool search --name \"" (or regexp accord-window-regexp) "\""))))

;;@TODO: use region when active?
(defun accord-send-message ()
  "Send message from accord buffer."
  (interactive)
  (let ((message (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
    (when (string-empty-p message) (user-error "Can't send empty message"))
    (gui-set-selection 'CLIPBOARD message)
    (accord-send-commands
     (accord--xdotool-clear-input)
     (accord--xdotool-paste)
     (accord--xdotool-confirm))
    (undo-boundary)
    (erase-buffer)))

(defun accord--last-message ()
  "Return last sent message."
  ;; Clear in case we have something already stored in the clipboard
  (gui-set-selection 'CLIPBOARD nil)
  (accord-send-commands
   (accord--xdotool-open-last)
   (accord--xdotool-copy-input)
   "Escape")
  (when-let ((selection (gui-get-selection 'CLIPBOARD)))
    (substring-no-properties selection)))

(defun accord-delete-message (&optional noconfirm)
  "Delete last posted message.
If NOCONFIRM is non-nil, do not prompt user for confirmation."
  (interactive "P")
  (let (last)
    (unless noconfirm
      (setq last (or (accord--last-message) (user-error "Unable to delete last message"))))
    (when (or noconfirm (yes-or-no-p (format "Delete message?: %S" last)))
      (accord-send-commands
       (accord--xdotool-open-last)
       (accord--xdotool-clear-input)
       ;; Discord doesn't let you delete without confirming and the pop-up takes
       ;; some time to appear...
       (let ((accord-key-delay-time "300")) (accord--xdotool-confirm))
       (accord--xdotool-confirm)))))

(defun accord--reset-header-line ()
  "Reset the header-line."
  (setq header-line-format
        (substitute-command-keys
         (concat "\\<accord-mode-map>Accord buffer. "
                 "Send: `\\[accord-send-message]' "
                 "Edit: `\\[accord-edit-message]' "
                 "Delete: `\\[accord-delete-message]'"))))

(defun accord--edit-send (&rest _)
  "Advice before sending message used when editing a message.
FN is `accord-send-message'."
  (let ((message (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
    (when (string-empty-p message) (user-error "Can't send empty message"))
    (gui-set-selection 'CLIPBOARD message)
    (accord-send-commands
     (accord--xdotool-open-last)
     (accord--xdotool-clear-input)
     (accord--xdotool-paste)
     (accord--xdotool-confirm))
    (undo-boundary)
    (erase-buffer)
    (accord--reset-header-line)
    (advice-remove #'accord-send-message #'accord--edit-send)))

(defvar accord-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'accord-send-message)
    (define-key map (kbd "C-c C-e") 'accord-edit-message)
    (define-key map (kbd "C-c C-k") 'accord-delete-message)
    map))

(define-derived-mode accord-mode markdown-mode "accord"
  "Send messages to Discord from Emacs."
  (accord--reset-header-line))

(defun accord--edit-abort (&rest _)
  "Advice before sending message used when editing a message.
FN is `accord-delete-message'."
  (erase-buffer)
  (accord--reset-header-line)
  (advice-remove #'accord-delete-message #'accord--edit-abort)
  (advice-remove #'accord-send-message #'accord--edit-send))

;;;###autoload
(defun accord-edit-message ()
  "Edit last message."
  (interactive)
  (insert (accord--last-message))
  (goto-char (point-min))
  (setq header-line-format
        (substitute-command-keys
         (concat "\\<accord-mode-map>Accord buffer. "
                 "Send: `\\[accord-send-message]' "
                 "Abort Edit: `\\[accord-delete-message]'")))
  (advice-add #'accord-delete-message :override #'accord--edit-abort)
  (advice-add #'accord-send-message :override #'accord--edit-send))

;;;###autoload
(defun accord-last-channel ()
  "Choose last channel."
  (interactive)
  (accord-send-commands "key" "--delay" "80" "ctrl+k" "Return"))

(defun accord-channel--scroll (direction)
  "Scroll channel in DIRECTION."
  (interactive)
  (accord-send-commands "key" "--delay" "80" direction))

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
(defun accord-channel-mark-read ()
  "Mark channel as read."
  (interactive)
  (accord-send-commands "key" "--delay" "80" "Escape"))

;;;###autoload
(defun accord-channel-goto-unread ()
  "Goto unread first unread message in channel."
  (interactive)
  (accord-send-commands "key" "--delay" "80" "Shift+Page_Up"))

(defun accord--select (entity direction)
  "Choose previous ENTITY in DIRECTION.
ENTITY may be either `server` or `channel`."
  (interactive)
  (accord-send-commands "key" "--delay" "100"
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
(defun accord-search-channel (&optional query)
  "Select channel by QUERY."
  (interactive)
  (let ((search (or query (read-string "accord channel: "))))
    (accord-send-commands
     "key" "--delay" "80" "ctrl+k"
     "key" "--delay" "20"
     (cdr (mapcar (lambda (char) (replace-regexp-in-string " " "space" char))
                  (split-string search "")))
     (accord--xdotool-confirm))))

;;;###autoload
(defun accord ()
  "Toggle accord buffer."
  (interactive)
  (if (string= (buffer-name) accord-buffer-name)
      (delete-window)
    (select-window
     (display-buffer-in-side-window (get-buffer-create accord-buffer-name) '((side . bottom))))
    (unless (derived-mode-p 'accord-mode) (accord-mode))))

(provide 'accord)

;;; accord.el ends here
