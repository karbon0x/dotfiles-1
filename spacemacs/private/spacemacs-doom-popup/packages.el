;;; packages.el --- spacemacs-doom-popup layer packages file for Spacemacs.
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
;; added to `spacemacs-doom-popup-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `spacemacs-doom-popup/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `spacemacs-doom-popup/pre-init-PACKAGE' and/or
;;   `spacemacs-doom-popup/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst spacemacs-doom-popup-packages
  '(shackle))


(defun spacemacs-doom-popup|init-shackle ()
  (use-package shackle
    :defines shackle-rules
    :config
    (shackle-mode 1)
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

    ;; :noesc    = Can't be closed with a single ESC
    ;; :nokill   = Won't be killed when closed
    ;; :modeline = Show the modeline
    (defvar doom-popup-rules
      '(("^\\*doom\\(:scratch\\)?\\*$" :noesc :nokill :modeline)
        ("^\\*doom.*\\*$"       :noesc :nokill)
        (ivy-occur-grep-mode    :noesc)
        (compilation-mode       :noesc)
        (comint-mode            :noesc :nokill)
        (eshell-mode            :noesc :nokill)
        (messages-buffer-mode          :nokill)
        (esup-mode              :noesc)
        (tabulated-list-mode    :noesc)))

    ;; There is no shackle-popup hook, so I hacked one in
    (advice-add 'shackle-display-buffer :around 'doom*popup-init)
    ;; Don't mess with popups
    (advice-add 'balance-windows        :around 'doom*save-popup)
    (advice-add 'doom/evil-window-move  :around 'doom*save-popup))


  ;;
  ;; Hacks
  ;;

  (after! help-mode
    ;; Following links in help buffers sometimes uses itself or other-window
    ;; (annoying!). It should only replace the buffer we opened the popup from. To
    ;; fix this these three button types need to be redefined so that the right
    ;; window is in focus before the link is followed.
    (define-button-type 'help-function-def
      :supertype 'help-xref
      'help-function (lambda (fun file)
                      (require 'find-func)
                      (when (eq file 'C-source)
                        (setq file (help-C-file-name (indirect-function fun) 'fun)))
                      (let ((location (find-function-search-for-symbol fun nil file)))
                        (doom/popup-close)
                        (switch-to-buffer (car location) nil t)
                        (if (cdr location)
                            (goto-char (cdr location))
                          (message "Unable to find location in file")))))

    (define-button-type 'help-variable-def
      :supertype 'help-xref
      'help-function (lambda (var &optional file)
                      (when (eq file 'C-source)
                        (setq file (help-C-file-name var 'var)))
                      (let ((location (find-variable-noselect var file)))
                        (doom/popup-close)
                        (switch-to-buffer (car location) nil t)
                        (if (cdr location)
                            (goto-char (cdr location))
                          (message "Unable to find location in file")))))

    (define-button-type 'help-face-def
      :supertype 'help-xref
      'help-function (lambda (fun file)
                      (require 'find-func)
                      (let ((location
                              (find-function-search-for-symbol fun 'defface file)))
                        (doom/popup-close)
                        (switch-to-buffer (car location) nil t)
                        (if (cdr location)
                            (goto-char (cdr location))
                          (message "Unable to find location in file")))))))



;;; packages.el ends here
