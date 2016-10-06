;;; packages.el --- spacemacs-doom-theme layer packages file for Spacemacs.
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
;; added to `spacemacs-doom-themes-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `doom-theme/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `doom-theme/pre-init-PACKAGE' and/or
;;   `doom-theme/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst spacemacs-doom-themes-packages
  '(
    doom-themes
    minimap))

(defun spacemacs-doom-themes/init-doom-themes ()
  (use-package doom-themes
    :config
    (load-theme 'doom-one t)
    ;;; OPTIONAL
    ;; brighter source buffers
    (add-hook 'find-file-hook 'doom-buffer-mode)
    ;; brighter minibuffer when active
    (add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)
    ;; mode-line is unimportant in help/compile windows
    (add-hook 'help-mode-hook 'doom-hide-mode-line-mode)
    (add-hook 'compilation-mode-hook 'doom-hide-mode-line-mode)
    (add-hook 'messages-buffer-mode-hook 'doom-hide-mode-line-mode)
    (with-current-buffer "*Messages*" (doom-hide-mode-line-mode +1))
    (add-hook 'minimap-sb-mode-hook 'doom-hide-mode-line-mode)))


(defun spacemacs-doom-themes/init-minimap ()
  (use-package minimap
    :config
    (setq minimap-window-location 'right
          minimap-recenter-type 'middle
          minimap-major-modes '(prog-mode enh-ruby-parent-mode)
          minimap-hide-fringes t)
    (spacemacs|add-toggle minimap
      :status minimap-mode
      :on (minimap-mode)
      :off (minimap-mode -1)
      :documentation "Enable minimap window"
      :evil-leader "tM")))

;;; doom.el ends here
