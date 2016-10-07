;;; packages.el --- spacemacs-doom layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Faust <jf019013@M1600562>
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
;; added to `spacemacs-doom-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `spacemacs-doom/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `spacemacs-doom/pre-init-PACKAGE' and/or
;;   `spacemacs-doom/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst spacemacs-doom-packages
  '(
    beacon
    hideshow
    hl-line
    linum
    ))

(defun spacemacs-doom/init-beacon ()
  (use-package beacon
    :config
    (beacon-mode +1)
    (setq beacon-color (face-attribute 'highlight :background nil t)
          beacon-blink-when-buffer-changes t
          beacon-blink-when-point-moves-vertically 10)))

(defun spacemacs-doom/init-hideshow ()
  (use-package hideshow
    :commands (hs-minor-mode hs-toggle-hiding hs-already-hidden-p)
    :config (setq hs-isearch-open t)
    :init
    (advice-add 'evil-toggle-fold :before 'doom*load-hs-minor-mode)
    ;; Prettify code folding in emacs
    (define-fringe-bitmap 'hs-marker [16 48 112 240 112 48 16] nil nil 'center)
    (defface hs-face '((t (:background "#ff8")))
      "Face to hightlight the ... area of hidden regions"
      :group 'hideshow)
    (defface hs-fringe-face '((t (:foreground "#888")))
      "Face used to highlight the fringe on folded regions"
      :group 'hideshow)
    (setq hs-set-up-overlay
          (lambda (ov)
            (when (eq 'code (overlay-get ov 'hs))
              (let* ((marker-string "*")
                    (display-string (concat " " (all-the-icons-octicon "ellipsis" :v-adjust 0) " "))
                    (len (length display-string)))
                (put-text-property 0 1 'display
                                  (list 'right-fringe 'hs-marker 'hs-fringe-face)
                                  marker-string)
                (put-text-property 0 1 'face 'hs-face display-string)
                (put-text-property (1- len) len 'face 'hs-face display-string)
                (put-text-property 1 (1- len)
                                  'face `(:inherit hs-face :family ,(all-the-icons-octicon-family) :height 1.2)
                                  display-string)
                (overlay-put ov 'before-string marker-string)
                (overlay-put ov 'display display-string)))))))

(defun spacemacs-doom/init-hl-line ()
  (use-package hl-line
    :init (add-hook 'prog-mode-hook 'hl-line-mode)
    :config
    ;; Stickiness doesn't seem to play nice in emacs 25+
    (setq hl-line-sticky-flag nil
          global-hl-line-sticky-flag nil)

    ;; Remember whether hl-line was initially on or off in the current buffer
    (defvar-local doom--hl-line-mode nil)
    (defun doom|hl-line-on ()  (if doom--hl-line-mode (hl-line-mode +1)))
    (defun doom|hl-line-off () (if doom--hl-line-mode (hl-line-mode -1)))
    (add-hook! hl-line-mode (if hl-line-mode (setq doom--hl-line-mode +1)))))

(defun spacemacs-doom/init-linum ()
  (use-package linum
    :preface
    (defvar linum-current-line 1 "Current line number.")
    (defvar linum-border-width 4 "Border width for linum.")

    (defface linum-current-line
      `((t :inherit doom-linum
           :background "#262c34"
          ))
      "Face for displaying the current line number."
      :group 'linum)

    (defadvice linum-update (before advice-linum-update activate)
      "Set the current line."
      (setq linum-current-line (line-number-at-pos)
            ;; It's the same algorithm that linum dynamic. I only had added one
            ;; space in front of the first digit.
            linum-border-width (number-to-string
                                (length
                                 (number-to-string
                                  (count-lines (point-min) (point-max)))))))

    (defun linum-highlight-current-line (line-number)
      "Highlight the current line number using `linum-current-line' face."
      (let ((face (if (= line-number linum-current-line)
                      'doom-nlinum-highlight
                    'linum)))
        (propertize (format (concat "%" linum-border-width "d ") line-number)
                    'face face)))
    :init
    (when dotspacemacs-line-numbers
      (add-hook 'prog-mode-hook 'linum-mode)
      (add-hook 'text-mode-hook 'linum-mode))
    (spacemacs|add-toggle line-numbers
      :mode linum-mode
      :documentation "Show the line numbers."
      :evil-leader "tn")
    :config
    (setq linum-format 'linum-highlight-current-line)))

;;; packages.el ends here
