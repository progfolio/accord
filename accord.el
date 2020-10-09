;;; accord.el --- Lazy Emacs to Discord Interface  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile (require 'subr-x))

;;; Custom Options
(defgroup accord nil
  "Lazy Emacs to Discord Interface"
  :group 'convenience
  :prefix "accord-")

(defcustom accord-window-regexp "(#|@).*Discord"
  "Regexp to identify Discord window."
  :type 'string)

(defmacro accord-save-window (&rest body)
  "Restore focus to current window after executing BODY."
  (declare (indent defun))
  (let ((current (make-symbol "current")))
    `(let ((,current (string-trim (shell-command-to-string "xdotool getwindowfocus"))))
       (unwind-protect
           (progn ,@body)
         (call-process "xdotool" nil (get-buffer-create "*test*") nil "windowactivate" ,current)))))

(defun accord--window-by-name (&optional regexp)
  "Return window ID for window name matching REGEXP."
  (string-trim (shell-command-to-string
                (concat "xdotool search --name \"" (or regexp accord-window-regexp) "\""))))

(defun accord--focus-window (id)
  "Focus window with ID."
  (call-process "/usr/bin/xdotool" nil (get-buffer-create "*test*") t "windowactivate" id))


(defun accord--send-keys (keys &optional regexp)
  "Send KEYS to window matching REGEXP."
  (accord-save-window
    (accord--focus-window (accord--window-by-name regexp))
    (apply #'call-process `("xdotool" nil ,(get-buffer-create "*test*") nil "key" ,@keys))))

(defun accord--clear-input ()
  "Clear Discord text input area."
  (accord--send-keys '("ctrl+a" "Delete")))

(defun accord-paste-in-window (&optional text regexp)
  "Paste TEXT in window matching name REGEXP.
If TEXT is nil, current clipboard is pasted."
  (when text
    (with-temp-buffer
      (insert text)
      (clipboard-kill-region (point-min) (point-max))))
  ;; Intentionally avoiding `accord--clear-input'
  ;; doing so will result in double paste if text
  ;; being pasted is equal to text in input area...
  (accord--send-keys '("ctrl+a" "Delete" "ctrl+v") regexp))

(defun accord-message (arg &optional message)
  "Send MESSAGE to discord.
If ARG is non-nil, just paste MESSAGE, but do not send."
  (interactive "P")
  (let ((message (or message
                     (when (region-active-p)
                       (buffer-substring-no-properties (mark) (point))))))
    (accord-paste-in-window message)
    (unless arg (accord--send-keys '("Return")))))

(defun accord-buffer-to-message (&optional arg)
  "Send buffer to Discord. ARG is passed to `accord-message'."
  (interactive "P")
  (accord-message arg (buffer-substring-no-properties (point-min) (point-max))))

(defun accord-empty-buffer-p ()
  "Return t if current buffer is empty (whitespace does not count)."
  (string-empty-p (string-trim (buffer-string))))

(defun accord-send-message ()
  "Send message from accord buffer."
  (interactive)
  (when (accord-empty-buffer-p)
    (user-error "Can't send empty message"))
  (accord-buffer-to-message)
  (erase-buffer))

(defun accord--last-message ()
  "Return last sent message."
  (accord--clear-input)
  (accord--send-keys '("Up" "ctrl+a" "ctrl+c" "Escape"))
  (substring-no-properties (gui-get-selection 'CLIPBOARD)))


;;@TODO: this may need some tweaking with the delay time.
;;Discord doesn't let you delete without confirming and the pop-up takes some time to appear...
(defun accord-delete-message (&optional noconfirm)
  "Delete last posted message.
If NOCONFIRM is non-nil, do not prompt user for confirmation."
  (interactive "P")
  (let ((last-message (accord--last-message)))
    ;;clear discord input first
    (when (or noconfirm (yes-or-no-p (format "Delete message?: %S" last-message)))
      (accord--send-keys '("--delay" "100" "Up" "ctrl+a" "Delete" "Return" "Return")))))

;;;###autoload
(defun accord ()
  "Select accord buffer."
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create "* accord *"))
  (accord-mode))

(define-minor-mode accord-mode
  "Send messages to Discord from Emacs."
  :lighter " accord"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'accord-send-message)
            (define-key map (kbd "C-c C-k") 'accord-delete-message)
            map)
  (let ((original header-line-format))
    (if accord-mode
        (setq header-line-format
              (substitute-command-keys
               (concat "\\<accord-mode-map>Accord buffer. "
                       "Send message `\\[accord-send-message]' "
                       "Delete last message `\\[accord-delete-message]'")))
      (setq header-line-format original))))

(provide 'accord)

;;; accord.el ends here
