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

(defcustom accord-buffer-name "*accord*"
  "Name of the accord buffer."
  :type 'string)

;;; Variables
(defun accord--current-window ()
  "Return ID for currently focused window."
  (string-trim (shell-command-to-string "xdotool getwindowfocus")))

;;@TODO: doesn't really work how we want it to.
;; just because the process has stopped, does not mean the keys are done being sent?
;; seems to suffer from race conditions.
(defmacro accord-save-window (&rest body)
  "Restore focus to current window after executing BODY."
  (declare (indent defun))
  (let ((current (make-symbol "current")))
    `(let ((,current (accord--current-window)))
       (unwind-protect
           (progn ,@body)
         (call-process "xdotool" nil (get-buffer-create "*accord-process*") nil "windowactivate" ,current)))))

(defun accord--window-by-name (&optional regexp)
  "Return window ID for window name matching REGEXP."
  (string-trim (shell-command-to-string
                (concat "xdotool search --name \"" (or regexp accord-window-regexp) "\""))))

;;@TODO: use region when active?
(defun accord-send-message ()
  "Send message from accord buffer."
  (interactive)
  (let ((current (accord--current-window))
        (target (accord--window-by-name))
        (message (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
    (when (string-empty-p message) (user-error "Can't send empty message"))
    (gui-set-selection 'CLIPBOARD message)
    (call-process "xdotool" nil (get-buffer-create "*accord-process*") t "windowactivate" "--sync" target
                  ;;clear input
                  "key" "--delay" "100" "Escape" "ctrl+a" "Delete"
                  "key" "--delay" "100" "ctrl+v"
                  "key" "--delay" "20" "Return"
                  "windowactivate" current)
    (erase-buffer)))

(defun accord--last-message ()
  "Return last sent message."
  ;; Clear in case we have something already stored in the clipboard
  (let ((current (accord--current-window))
        (target (accord--window-by-name)))
    (gui-set-selection 'CLIPBOARD nil)
    (call-process "xdotool" nil (get-buffer-create "*accord-process*") t "windowactivate" "--sync" target
                  ;;clear input open last message
                  "key" "--delay" "100" "ctrl+a" "Delete" "Escape" "Up"
                  ;;copy message
                  "key" "--delay" "100" "ctrl+a" "ctrl+c" "Escape"
                  "windowactivate" current)
    (when-let ((selection (gui-get-selection 'CLIPBOARD)))
      (substring-no-properties selection))))

;;@TODO: this may need some tweaking with the delay time.
;;Discord doesn't let you delete without confirming and the pop-up takes some time to appear...
(defun accord-delete-message (&optional noconfirm)
  "Delete last posted message.
If NOCONFIRM is non-nil, do not prompt user for confirmation."
  (interactive "P")
  (let ((current (accord--current-window))
        (last-message (accord--last-message)))
    (unless last-message (user-error "Unable to delete last message"))
    (when (or noconfirm (yes-or-no-p (format "Delete message?: %S" last-message)))
      (call-process "xdotool" nil (get-buffer-create "*accord-process*") nil
                    ;;activate target window
                    "windowactivate" "--sync" (accord--window-by-name)
                    ;;send keys to delete
                    "key" "--delay" "100" "Up"
                    "key" "--delay" "100"  "ctrl+a" "Delete"
                    "key" "--delay" "200" "Return" "Return"
                    ;;refocus caller
                    "windowactivate" current))))

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
  (let ((current (accord--current-window))
        (message (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
    (when (string-empty-p message) (user-error "Can't send empty message"))
    (gui-set-selection 'CLIPBOARD message)
    (call-process "xdotool" nil (get-buffer-create "*accord-process*") nil
                  ;;activate target window
                  "windowactivate" "--sync" (accord--window-by-name)
                  ;;clear input open last message
                  "key" "--delay" "120" "ctrl+a" "Delete" "Escape" "Up"
                  ;;send keys to update
                  "key" "--delay" "15" "ctrl+a" "ctrl+v" "Return"
                  ;;refocus caller
                  "windowactivate" current))
  (erase-buffer)
  (accord--reset-header-line)
  (advice-remove #'accord-send-message #'accord--edit-send))

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
  (let ((current (accord--current-window)))
    (call-process "xdotool" nil (get-buffer-create "*accord-process*") nil
                  ;;activate target window
                  "windowactivate" "--sync" (accord--window-by-name)
                  ;;send keys to delete
                  "key" "--delay" "80" "ctrl+k" "Return"
                  ;;refocus caller
                  "windowactivate" current)))

;;;###autoload
(defun accord-select-channel ()
  "Select channel interactively."
  (interactive)
  (let ((current (accord--current-window))
        (search (read-string "accord channel: ")))
    (apply #'call-process
           `("xdotool" nil ,(get-buffer-create "*accord-process*") nil
             ;;activate target window
             "windowactivate" "--sync" ,(accord--window-by-name)
             ;;send keys to delete
             "key" "--delay" "80" "ctrl+k"
             "key" "--delay" "20" ,@(cdr (mapcar (lambda (char) (replace-regexp-in-string " " "space" char))
                                                 (split-string search "")))
             "key" "--delay" "40" "Return" "Escape" "Escape"
             ;;refocus caller
             "windowactivate" ,current))))

(define-minor-mode accord-mode
  "Send messages to Discord from Emacs."
  :lighter " accord"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'accord-send-message)
            (define-key map (kbd "C-c C-e") 'accord-edit-message)
            (define-key map (kbd "C-c C-k") 'accord-delete-message)
            map)
  (let ((original header-line-format))
    (if accord-mode
        (progn
          (setq header-line-format
                (substitute-command-keys
                 (concat "\\<accord-mode-map>Accord buffer. "
                         "Send: `\\[accord-send-message]' "
                         "Edit: `\\[accord-edit-message]' "
                         "Delete: `\\[accord-delete-message]'"))))
      (setq header-line-format original))))

;;;###autoload
(defun accord ()
  "Toggle accord buffer."
  (interactive)
  (if (string= (buffer-name) accord-buffer-name)
      (delete-window)
    (select-window
    (display-buffer-in-side-window (get-buffer-create accord-buffer-name) '((side . bottom))))
    (unless accord-mode (accord-mode))))

(provide 'accord)

;;; accord.el ends here
