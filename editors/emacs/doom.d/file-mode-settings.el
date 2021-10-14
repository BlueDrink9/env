;;; filetype-modes.el -*- lexical-binding: t; -*-

;; Use variable width fonts and soft wrap for all prose filetypes
;; TODO exclude text mode when evil-collection-magit-toggle-text-mode minor mode is active
(add-hook! 'text-mode-hook
            (mixed-pitch-mode 1)
            ;; (visual-line-motion-mode t)
            ;; (+word-wrap-mode t)
            ;; (setq doom-modeline-enable-word-count t)
            )
; Inherits from text-mode
(add-hook! 'yaml-mode-hook
            (mixed-pitch-mode 0)
            )

;; Use soft wraps instead of hard. Only enable if found to be necessary.
;; (remove-hook 'text-mode-hook #'auto-fill-mode)
;; (add-hook 'message-mode-hook #'word-wrap-mode)

;; Add underscore to word character definition.
(modify-syntax-entry ?_ "w")

;;; For lisp,
;; (add-hook! 'lisp-mode-hook (modify-syntax-entry ?_ "w"))

;;; Markdown
(add-hook! 'markdown-mode-hook
    (setq markdown-fontify-code-blocks-natively 1)
    (setq markdown-header-scaling 1)
  )
(setq markdown-fontify-code-blocks-natively 1)
(setq markdown-header-scaling 1)


;;; Org

(setq org-export-publishing-directory "./build")
(defun org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
  (unless pub-dir
    (setq pub-dir "build")
    (unless (file-directory-p pub-dir)
      (make-directory pub-dir)))
  (apply orig-fun extension subtreep pub-dir nil))
(advice-add 'org-export-output-file-name :around #'org-export-output-file-name-modified)
;; (setq org-publish-project-alist
;;   '(("all" :components ("html" "pdf")
;;      :publishing-directory "build")))


;; Auto update toc
(after! toc-org
  ;; enable in markdown, too
  (add-hook 'markdown-mode-hook 'toc-org-mode))
;; Set default latex packages for org doc
;; (after! org (add-to-list 'org-latex-default-packages-alist ...))

(after! company-bibtex
  ;; Allow completion with \cite
  (setq company-bibtex-org-citation-regex (format "%s\\|%s"
                                                  company-bibtex-org-citation-regex company-bibtex-latex-citation-regex))
  )
;; company-bibtex completion backend function doesn't load automatically, so
;; have to trigger it to laod for the relevant modes.
(use-package! company-bibtex
  :hook (org-mode latex-mode))
;; (use-package! company-bibtex
;;   :commands company-bibtex)
;; (dolist (mode (list 'org-mode 'latex-mode))
;;   ;; Have to trigger this in order to autoload company-bibtex
;;   (add-hook mode ((ignore-errors (company-bibtex)))))

;; Set bibliography from a locally-defined variable.
;; Put the following in a `.dir-locals.el` file in the directory you are working in.
;; ((text-mode . ((local-bib-path . "../../2021_Masters.bib"))))
(defun update-bibliography-path (local-bib-path)
  (setq-local company-bibtex-bibliography local-bib-path)
  ;; (add-to-list reftex-default-bibliography local-bib-path)
  (setq-local reftex-bib-path local-bib-path)
  (setq-local reftex-default-bibliography '(local-bib-path))
  ;; (add-to-list bibtex-completion-bibliography local-bib-path))
  )
;; (dolist (hook (list 'org-mode-hook 'latex-mode-hook))
;;   (add-hook! hook
(add-hook! 'org-mode-hook
    (unless (not (boundp 'local-bib-path))
      (update-bibliography-path 'local-bib-path)))
(add-hook! 'latex-mode-hook
    (unless (not (boundp 'local-bib-path))
      (update-bibliography-path 'local-bib-path)))
;; (mapc (lambda (hook)
;;         (add-hook hook
;;           (unless (not (boundp 'local-bib-path))
;;              (update-bibliography-path 'local-bib-path))))
;;       '(org-mode-hook latex-mode-hook))
;; If we want to set them manually in a .dir-locals.el file, need to mark them
;; as safe.
(put 'company-bibtex-bibliography 'safe-local-variable #'stringp)
(put 'reftex-default-bibliography 'safe-local-variable #'stringp)
(put 'bibtex-completion-bibliography 'safe-local-variable #'stringp)
(put 'local-bib-path 'safe-local-variable #'stringp)

;;; LaTeX
;; (setq org-latex-pdf-process (list "latexmk -shell-escape -f -pdfxe %f"))
(add-hook 'latex-mode-hook
          '(lambda ()
             (mapc
              (lambda (face)
                (set-face-attribute face nil :inherit 'fixed-pitch))
              (list 'font-latex-verbatim-face
                    'font-lock-keyword-face
                    ;; 'font-lock-sedate-face
                    'font-lock-function-name-face
                    'tex-verbatim
                    'font-latex-doctex-documentation-face
                    'font-latex-doctex-preprocessor-face
                    'TeX-error-description-help
                    'TeX-error-description-warning
                    'TeX-error-description-tex-said
                    ))))

;; Emacs for statistics (R)
(after! ess
  ;; (require 'ess-site)
  ;; (require 'ess-mode)
  ;;   (define-key evil-normal-state-map (kbd "<SPC-e>") 'ess-execute))
  ;; Set ESS options
  (setq
   ess-ask-for-ess-directory nil ; start R in the working directory by default
   ess-auto-width 'window
   ess-use-auto-complete nil
   ess-use-company 't
   ;; ess-r-package-auto-set-evaluation-env nil
   inferior-ess-same-window 't
   ess-indent-with-fancy-comments nil ; don't indent comments
   ess-eval-visibly t                 ; enable echoing input
   ;; ess-eval-empty t                   ; don't skip non-code lines.
   ;; ess-R-font-lock-keywords      ; font-lock, but not too much
   ))
