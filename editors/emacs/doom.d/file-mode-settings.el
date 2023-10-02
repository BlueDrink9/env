;;; filetype-modes.el -*- lexical-binding: t; -*-

;; Use variable width fonts and soft wrap for all prose filetypes
;; TODO exclude text mode when evil-collection-magit-toggle-text-mode minor mode is active
(add-hook! 'text-mode-hook
            (mixed-pitch-mode 1)
            ;; (visual-line-motion-mode t)
            ;; (+word-wrap-mode t)
            ;; (setq doom-modeline-enable-word-count t)
            (wc-mode 1)
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
;; Clear the annoying text insert on new markdown files.
(set-file-template! 'markdown-mode)

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

(setq org-ellipsis "↴")
(after! org
  (flyspell-mode t))
;;   (setq org-fontify-quote-and-verse-blocks nil
;;         org-fontify-whole-heading-line nil
;;         org-hide-leading-stars nil
;;         org-startup-indented nil))

;; Hides markup symbols until you enter the word
(add-hook! org-mode #'org-appear-mode)
(setq org-appear-autolinks t
      org-appear-autosubmarkers t
      org-appear-autoentities t
      org-appear-autokeywords t
      org-appear-delay 0)
;; The same, but for latex fragment previews
(add-hook! org-mode #'org-fragtog-mode)




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
(dolist (mode '(org-mode-hook LaTeX-mode-hook))
; Can't use add-hook! within a loop, so using add-hook
  (add-hook mode
            (lambda ()
              (when (boundp 'local-bib-path)
                (update-bibliography-path 'local-bib-path))
              ;; Disable concealing symbols etc.
              (prettify-symbols-mode -1)
              (TeX-fold-mode -1))))

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

;; ;;; LaTeX
;; (add-hook! 'latex-mode-hook
;; )

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
