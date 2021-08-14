;;; filetype-modes.el -*- lexical-binding: t; -*-

;; Use variable width fonts and soft wrap for all prose filetypes
(add-hook 'text-mode-hook
          (lambda ()
            (mixed-pitch-mode 1)
            (visual-line-mode)
            ;; (setq doom-modeline-enable-word-count t)
            ))

;; Use soft wraps instead of hard. Only enable if found to be necessary.
;; (remove-hook 'text-mode-hook #'auto-fill-mode)
;; (add-hook 'message-mode-hook #'word-wrap-mode)

;; Add underscore to word character definition.
(modify-syntax-entry ?_ "w")

;;; For lisp,
;; (add-hook! 'lisp-mode-hook (modify-syntax-entry ?_ "w"))

;;; Markdown
(add-hook! 'markdown-mode-hook
  ( progn
    (setq markdown-fontify-code-blocks-natively 1)
    (setq markdown-header-scaling 1)
    )
  )
(setq markdown-fontify-code-blocks-natively 1)
(setq markdown-header-scaling 1)

;; Auto update toc
(after! toc-org
  ;; enable in markdown, too
  (add-hook 'markdown-mode-hook 'toc-org-mode))
;; Set default latex packages for org doc
;; (after! org (add-to-list 'org-latex-default-packages-alist ...))

;; Emacs for statistics (R)
(after! ess
  ;; (require 'ess-site)
  ;; (require 'ess-mode)
  ;;   (define-key evil-normal-state-map (kbd "<SPC-e>") 'ess-execute))

  ;; Set ESS options
  (setq
   ess-auto-width 'window
   ess-use-auto-complete nil
   ess-use-company 't
   ;; ess-r-package-auto-set-evaluation-env nil
   inferior-ess-same-window 't
   ess-indent-with-fancy-comments nil ; don't indent comments
   ess-eval-visibly t                 ; enable echoing input
   ;; ess-eval-empty t                   ; don't skip non-code lines.
   ess-ask-for-ess-directory nil ; start R in the working directory by default
   ;; ess-R-font-lock-keywords      ; font-lock, but not too much
   (quote
    ((ess-R-fl-keyword:modifiers)
     (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:keywords . t)
     (ess-R-fl-keyword:assign-ops  . t)
     (ess-R-fl-keyword:constants . 1)
     (ess-fl-keyword:fun-calls . t)
     (ess-fl-keyword:numbers)
     (ess-fl-keyword:operators . t)
     (ess-fl-keyword:delimiters)
     (ess-fl-keyword:=)
     (ess-R-fl-keyword:F&T)))))
