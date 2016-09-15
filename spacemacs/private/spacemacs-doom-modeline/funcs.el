
(defun spacemacs-doom-modeline//restore-powerline (buffer)
  (with-current-buffer buffer
    (setq-local mode-line-format (default-value (doom-mode-line)))
    (powerline-set-selected-window)
    (powerline-reset)))

(defun spacemacs-doom-modeline//set-powerline-for-startup-buffers ()
  "Set the powerline for buffers created when Emacs starts."
  (message "setting modeline in hook")
  (dolist (buffer '("*Messages*" "*spacemacs*" "*Compile-Log*"))
    (when (get-buffer buffer)
      (message "setting modeline")
      (spacemacs//restore-powerline buffer))))
