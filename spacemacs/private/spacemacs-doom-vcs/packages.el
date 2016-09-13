;;; packages.el --- spacemacs-doom-vcs layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Faust <jf019013@m1600562.cerner.com>
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
;; added to `spacemacs-doom-vcs-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `spacemacs-doom-vcs/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `spacemacs-doom-vcs/pre-init-PACKAGE' and/or
;;   `spacemacs-doom-vcs/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst spacemacs-doom-vcs-packages
  '(
    diff-mode
    fringe-helper
    git-gutter-fringe
    git-gutter))

(defun spacemacs-doom-vcs/init-diff-mode ()
  (use-package diff-mode
    :defer t
    :config
    (evilified-state-evilify diff-mode diff-mode-map
      "j" 'diff-hunk-next
      "k" 'diff-hunk-prev)))

(defun spacemacs-doom-vcs/init-fringe-helper ()
  (use-package fringe-helper))

(defun spacemacs-doom-vcs/init-git-gutter-fringe ()
  (use-package git-gutter-fringe
    :init
    (setq shackle-rules
          `(;; Util
            ("^\\*.+-Profiler-Report .+\\*$" :align below :size 0.3 :regexp t)
            ("*esup*"            :align below :size 0.4 :noselect t)
            ("*minor-modes*"     :align below :size 0.5 :noselect t)
            ("*eval*"            :align below :size 16  :noselect t)
            ;; Doom
            (" *doom*"           :align below :size 35  :select t)
            ("^\\*doom:.+\\*$"   :align below :size 35  :select t :regexp t)
            ("^\\*doom.+\\*$"    :align below :size 12  :noselect t :regexp t)
            ;; Emacs
            ("*Pp Eval Output*"  :align below :size 0.3)
            ("*Apropos*"         :align below :size 0.3)
            ("*Backtrace*"       :align below :size 25  :noselect t)
            ("*Completions*"     :align below :size 30  :noselect t)
            ("*Help*"            :align below :size 16  :select t)
            ("*Messages*"        :align below :size 15  :select t)
            ("*Warnings*"        :align below :size 10  :noselect t)
            (compilation-mode    :align below :size 15  :noselect t)
            (eww-mode            :align below :size 30  :select t)
            ("*command-log*"     :align right :size 28  :noselect t)
            ;; vcs
            ("*vc-diff*"         :align below :size 15  :noselect t)
            ("*vc-change-log*"   :align below :size 15  :select t)
            (vc-annotate-mode    :same t)))
    (defmacro def-popup! (&rest params)
      `(push ',params shackle-rules))
    (def-popup! "^\\*git-gutter.+\\*$" :align below :size 15 :noselect t :regexp t)
    :config
    (define-fringe-bitmap 'git-gutter-fr:added
      [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
      nil nil 'center)
    (define-fringe-bitmap 'git-gutter-fr:modified
      [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
      nil nil 'center)
    (define-fringe-bitmap 'git-gutter-fr:deleted
      [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
      nil nil 'center)))

(defun spacemacs-doom-vcs/init-git-gutter ()
  (use-package git-gutter
    :commands (git-gutter-mode doom/vcs-next-hunk doom/vcs-prev-hunk
              doom/vcs-show-hunk doom/vcs-stage-hunk doom/vcs-revert-hunk)
    :init
    (add-hook! (text-mode prog-mode conf-mode) 'git-gutter-mode)
    :config
      ;; TODO: Copied from spacemacs-doom-popup

    ;; Refreshing git-gutter
    (advice-add 'evil-force-normal-state :after 'git-gutter)
    (add-hook 'focus-in-hook 'git-gutter:update-all-windows)

    (defalias 'doom/vcs-next-hunk    'git-gutter:next-hunk)
    (defalias 'doom/vcs-prev-hunk    'git-gutter:previous-hunk)
    (defalias 'doom/vcs-show-hunk    'git-gutter:popup-hunk)
    (defalias 'doom/vcs-stage-hunk   'git-gutter:stage-hunk)
    (defalias 'doom/vcs-revert-hunk  'git-gutter:revert-hunk)))


;;; packages.el ends here
