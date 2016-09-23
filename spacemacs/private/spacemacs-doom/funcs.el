;;;###autoload
(defun doom*helm-replace-prompt (plist)
  (if (keywordp (car plist))
      (setq plist (plist-put plist :prompt helm-global-prompt))
    (setcar (nthcdr 2 plist) helm-global-prompt))
  plist)

;;;###autoload
(defun doom*helm-hide-header (source &optional force)
  (doom-hide-mode-line-mode +1))

;;;###autoload
(defun doom*helm-hide-source-header-maybe ()
  (if (<= (length helm-sources) 1)
      (set-face-attribute 'helm-source-header nil :height 0.1 :foreground "#111111")
    (set-face-attribute 'helm-source-header nil :height 1.0 :foreground doom-helm-header-fg)))
