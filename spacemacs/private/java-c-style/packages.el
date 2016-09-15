;;; packages.el --- java-c-style layer packages file for Spacemacs.
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
;; added to `java-c-style-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `java-c-style/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `java-c-style/pre-init-PACKAGE' and/or
;;   `java-c-style/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst java-c-style-packages
  '(
    google-c-style))

(defun java-c-style/init-google-c-style ()
  (use-package google-c-style
    :config
    (progn
      ;; Java coding conventions
      (defconst google-java-style
        `((c-recognize-knr-p . nil)
          (c-enable-xemacs-performance-kludge-p . t) ; speed up indentation in XEmacs
          (c-basic-offset . 2)
          (indent-tabs-mode . nil)
          (c-comment-only-line-offset . 0)
          (c-hanging-braces-alist . ((defun-open after)
                                    (defun-close before after)
                                    (class-open after)
                                    (class-close before after)
                                    (inexpr-class-open after)
                                    (inexpr-class-close before)
                                    (namespace-open after)
                                    (inline-open after)
                                    (inline-close before after)
                                    (block-open after)
                                    (block-close . c-snug-do-while)
                                    (extern-lang-open after)
                                    (extern-lang-close after)
                                    (statement-case-open after)
                                    (substatement-open after)))
          (c-hanging-colons-alist . ((case-label)
                                    (label after)
                                    (access-label after)
                                    (member-init-intro before)
                                    (inher-intro)))
          (c-hanging-semi&comma-criteria
          . (c-semi&comma-no-newlines-for-oneline-inliners
              c-semi&comma-inside-parenlist
              c-semi&comma-no-newlines-before-nonblanks))
          (c-indent-comments-syntactically-p . t)
          (comment-column . 40)
          (c-indent-comment-alist . ((other . (space . 2))))
          (c-cleanup-list . (brace-else-brace
                            brace-elseif-brace
                            brace-catch-brace
                            empty-defun-braces
                            defun-close-semi
                            list-close-comma
                            scope-operator))
          (c-offsets-alist . ((arglist-intro google-c-lineup-expression-plus-4)
                              ;; (arglist-cont-nonempty 4)
                              (func-decl-cont . ++)
                              (member-init-intro . ++)
                              (inher-intro . ++)
                              (comment-intro . 0)
                              ;; make continuations 4 spaces from start of line
                              (arglist-cont . 4)
                              (arglist-cont-nonempty . 4)
                              (arglist-close . 4)
                              (topmost-intro . 0)
                              (block-open . 0)
                              (inline-open . 0)
                              (substatement-open . 0)
                              (statement-cont
                              .
                              (,(when (fboundp 'c-no-indent-after-java-annotations)
                                  'c-no-indent-after-java-annotations)
                                ,(when (fboundp 'c-lineup-assignments)
                                  'c-lineup-assignments)
                                ++))
                              (label . /)
                              (case-label . +)
                              (statement-case-open . +)
                              (statement-case-intro . +) ; case w/o {
                              (access-label . /)
                              (innamespace . 0))))
        "Google Java Programming Style.")
      (add-hook 'c-mode-common-hook
            (lambda()
              (subword-mode)
              (c-add-style "Google-Java" google-java-style t)
              (google-make-newline-indent)
              (setq c-basic-offset 2
                    tab-width 2)))
      (setq fci-rule-column 100))))


;;; packages.el ends here
