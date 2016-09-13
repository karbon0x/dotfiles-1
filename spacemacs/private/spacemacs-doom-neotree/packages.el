;;; packages.el --- spacemacs-doom-neotree layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Faust <jf019013@m1600562.northamerica.cerner.net>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `spacemacs-doom-neotree-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `spacemacs-doom-neotree/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `spacemacs-doom-neotree/pre-init-PACKAGE' and/or
;;   `spacemacs-doom-neotree/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst spacemacs-doom-neotree-packages
  '(
    neotree))

(defun spacemacs-doom-neotree/init-neotree ()
  (use-package neotree
    :commands (neotree-show
              neotree-hide
              neotree-toggle
              neotree-dir
              neotree-find
              neo-global--with-buffer
              neo-global--window-exists-p)
    :init
    (progn
      (message "initializing neotree")
      (setq neo-create-file-auto-open t
            neo-auto-indent-point t
            neo-mode-line-type 'none
            neo-persist-show nil
            neo-window-width 26
            neo-show-updir-line nil
            neo-auto-indent-point t
            neo-theme 'nerd ; fallback
            neo-banner-message nil)
      (defun neotree-find-project-root ()
        (interactive)
        (if (neo-global--window-exists-p)
            (neotree-hide)
          (let ((origin-buffer-file-name (buffer-file-name)))
            (neotree-find (projectile-project-root))
            (neotree-find origin-buffer-file-name))))
      (message "defining keybindings")
      (spacemacs/set-leader-keys
        "ft" 'neotree-toggle
        "pt" 'neotree-find-project-root))
    :config
    (evil-set-initial-state 'neotree-mode 'motion)
    (add-hook 'neo-after-create-hook 'doom-hide-mode-line-mode)

    ;; A custom and simple theme for neotree
    (advice-add 'neo-buffer--insert-fold-symbol :override 'doom*neo-insert-fold-symbol)
    ;; Shorter pwd in neotree
    (advice-add 'neo-buffer--insert-root-entry :filter-args 'doom*neo-insert-root-entry)
    ;; Don't ask for confirmation when creating files
    (advice-add 'neotree-create-node :around 'doom*neotree-create-node)
    ;; Prevents messing up the neotree buffer on window changes
    (advice-add 'doom/evil-window-move :around 'doom*save-neotree)

    ;; Minimize 'border' between windows (won't work in hook)
    (defun doom*neotree-no-fringes () (set-window-fringes neo-global--window 1 0))
    (advice-add 'neo-global--select-window :after 'doom*neotree-no-fringes)

    (add-hook! neotree-mode
      (set (make-local-variable 'hl-line-sticky-flag) t)
      (setq line-spacing 2)
      (hl-line-mode +1))
    (map! :map neotree-mode-map
          :m "\\\\" 'evil-window-prev
          "ESC ESC" 'neotree-hide
          "q"       'neotree-hide
          [return]  'neotree-enter
          "RET"     'neotree-enter
          :m "J"    'neotree-select-next-sibling-node
          :m "K"    'neotree-select-previous-sibling-node
          :m "H"    'neotree-select-up-node
          :m "L"    'neotree-select-down-node
          "v"       'neotree-enter-vertical-split
          "s"       'neotree-enter-horizontal-split
          "c"       'neotree-create-node
          "d"       'neotree-delete-node
          "g"       'neotree-refresh
          "r"       'neotree-rename-node
          "R"       'neotree-change-root)))


;;; packages.el ends here
