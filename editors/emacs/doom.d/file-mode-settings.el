;;; filetype-modes.el -*- lexical-binding: t; -*-

;; Use variable width fonts and soft wrap for all prose filetypes
(add-hook 'text-mode-hook
          (lambda ()
            (variable-pitch-mode 1)
            (visual-line-mode)
            ))

;; Use soft wraps instead of hard. Only enable if found to be neccessary.
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

;;; Org
(add-hook 'org-mode-hook
            '(lambda ()
               (mapc
                (lambda (face)
                  (set-face-attribute face nil :inherit 'fixed-pitch))
                (list 'org-code
                      'org-link
                      'org-block
                      'org-table
                      'org-block-begin-line
                      'org-block-end-line
                      'org-meta-line
                      'org-document-info-keyword))))
