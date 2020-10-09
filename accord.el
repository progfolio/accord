;;; accord.el --- Lazy Emacs to Discord Interface  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile (require 'subr-x))

(defun accord-paste-in-window (name &optional text keys)
  "Paste TEXT in window matching NAME regexp.
Note NAME is flavor of regexp used by xdotool.
If TEXT is nil, current clipboard is pasted. If KEYS is a non-nil
sequence of xdotool keys, they are sent to the window after pasting TEXT."
  (let* ((current (string-trim (shell-command-to-string "xdotool getwindowfocus")))
         (target (string-trim (shell-command-to-string (concat "xdotool search --name \""
                                                               name "\"")))))
    ;; put text on clipboard
    (when text
      (with-temp-buffer
        (insert text)
        (clipboard-kill-region (point-min) (point-max))))
    (shell-command (concat "xdotool windowactivate "
                           target
                           " key ctrl+a Delete ctrl+v "
                           keys
                           " windowactivate " current))))

(defun accord-message (arg &optional message)
  "Send MESSAGE to discord.
If ARG is non-nil, just paste MESSAGE, but do not send."
  (interactive "P")
  (let ((regexp "(#|@).*Discord")
        (message (or message
                     (when (region-active-p)
                       (buffer-substring-no-properties (mark) (point))))))
    (accord-paste-in-window regexp "")
    (accord-paste-in-window regexp message (unless arg "Return"))))

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
            map)
  (let ((original header-line-format))
    (if accord-mode
        (setq header-line-format
              (substitute-command-keys
               "\\<accord-mode-map>Accord buffer. Send message to Discord `\\[accord-send-message]'"))
      (setq header-line-format original))))

(provide 'accord)

;;; accord.el ends here
