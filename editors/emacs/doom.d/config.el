;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
; TODO: autobracket
; TODO: jumping to mark should put mark in middle of view
; TODO: viminfo-style savings (persistend jump list, registers)
; TODO: Bacsspace undoes tab
; magit mappings to replace c-C? Or just create another mapping that sends c-C all the time. But a shortcut for magit to do c-c c-c or c-c c-k would be helpful.

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
;; add-hook! takes either a quoted hook func, list, or unquoted major mode!

;; (unless (server-running-p) (server-start))

(setq script_dir (file-name-directory (or load-file-name buffer-file-name)))
(load-file (concat script_dir "bindings.el"))
(load-file (concat script_dir "aliases.el"))
(load-file (concat script_dir "appearance.el"))
(load-file (concat script_dir "file-mode-settings.el"))
(load-file (concat script_dir "../abbrev_defs.el"))
(use-package! s)
;; (load-file (concat script_dir "spellcheck.el"))
;; Remember, you do not need to run 'doom sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
; (setq user-full-name "John Doe"
;       user-mail-address "john@doe.com")


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org")

(setq abbrev-file-name             ;; tell emacs where to read abbrev
      (concat script_dir "../abbrev_defs"))
(add-hook 'doom-first-input-hook #'abbrev-mode)

;; Uses # for comments, generally a good fit for files without a specified file extension.
(setq-default major-mode 'conf-mode)
(setq-default initial-major-mode 'conf-mode)

;; Use " register by default, instead of system. System register is still
;; accessible via evil + and * registers.
;; Equivalent of vim `set clipboard=unnamed`.
(setq select-enable-clipboard nil)

(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width
(setq enable-dir-local-variables t)
;; Will ask about varibles it doesn't know about.
(setq enable-local-variables t)

;; Word wrap
(set-default 'truncate-lines nil)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq +word-wrap-extra-indent 'single)


;; Doom disables auto-save/backup by default.
(setq auto-save-default t
      make-backup-files t)
;; Scroll-off
(setq scroll-margin 5)
;; Autosaves
(after! super-save
  ;; (setq super-save-remote-files nil)
  ;; (setq super-save-exclude '(".gpg"))
  (add-to-list 'super-save-hook-triggers 'evil-insert-state-exit-hook))
(add-hook! 'change-major-mode-hook
  (super-save-mode 1))


;; Evil-style bindings in minibuffer (esc, c-n etc)
(setq evil-collection-setup-minibuffer t)
(setq evil-want-minibuffer t)
(add-hook! 'minibuffer-setup-hook
      (setq truncate-lines nil))
;; (add-hook 'minibuffer-setup-hook 'evil-collection-minibuffer-insert)
(setq evil-repeat-move-cursor nil)
(setq evil-cross-lines t)
(setq evil-split-window-below t)
(setq evil-vsplit-window-right t)
(setq evil-kbd-macro-suppress-motion-error "replay")
(setq evil-disable-insert-state-bindings nil)
;; Needs to be set in init.el, before evil loads.
;; Also currently has an issue with deleting the newline on cc/dd, see doom#2447
;; (setq evil-respect-visual-line-mode t)
(setq evil-collection-want-unimpaired-p t)

;; Disable smartparens. Can't be done another way because it is a default package.
;; (remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
;; or: from an old github issue:
(after! smartparens (smartparens-global-mode -1))
;;
(sp-local-pair 'lua-mode "function" "end")
;; Disable autopairing unless before newline
;; (sp-pair "*" nil :unless sp-point-before-eol-p)
;; :actions '(wrap autoskip navigate)

;; Insert extra newline between brackets if you add one yourself.
;; Similar to inoremap (<CR> (<CR>)<Esc>O
(defun indent-between-pair (&rest _ignored)
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(sp-local-pair 'prog-mode "{" nil :post-handlers '((indent-between-pair "RET")))
(sp-local-pair 'prog-mode "[" nil :post-handlers '((indent-between-pair "RET")))
(sp-local-pair 'prog-mode "(" nil :post-handlers '((indent-between-pair "RET")))

;; Fancy bullets in org mode, heavy plugin so remove.
(remove-hook 'org-mode-hook #'org-superstar-mode)
;; Syntax-highlighting in code blocks
(setq org-src-fontify-natively t)

;; (use-package! org-pretty-table
;;   :commands (org-pretty-table-mode global-org-pretty-table-mode))
;;

(after! smart-tab
  (setq smart-tab-using-hippie-expand t)
  (global-smart-tab-mode 1)
  )

(remove-hook 'doom-first-input-hook #'evil-snipe-mode)

;; Use cx for exchange mapping
(after! evil-exchange
  (evil-exchange-cx-install))
;; Only highlight targets when f,F,T,t pressed.
(after! evil-quickscope
  (global-evil-quickscope-mode 1)
  )


;; Include gitgutter jumps in jump list.
(evil-add-command-properties #'git-gutter:next-hunk :jump t)
(evil-add-command-properties #'git-gutter:previous-hunk :jump t)

;; More granular undos (i.e. not just an entire insert)
(setq evil-want-fine-undo t)
(add-hook! 'evil-local-mode-hook 'undo-tree-mode)
;; Persist registers
(setq savehist-additional-variables '(register-alist))
(setq undo-limit 80000000)  ; 80 Mb
(setq undo-tree-auto-save-history t)
;; Compressing undo history
(defadvice undo-tree-make-history-save-file-name
  (after undo-tree activate)
  (setq ad-return-value (concat ad-return-value ".gz")))

;; Lazy-load. Seems to be interferring with doom-reload.
;; (use-package! vlf-setup
;;   :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

;; If complete, TAB first tries to indent the current line, and if the line
;; was already indented, then try to complete the thing at point.
;; If nil, hitting TAB indents the current line if point is at the left margin
;; or in the line's indentation, otherwise it inserts a "real" TAB character.
(setq tab-always-indent 'complete)

(after! vertico
  ;; Case insensitve searching
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)
  (use-package! fussy
    :init (setq fussy-score-fn 'fussy-hotfuzz-score)
           (push 'fussy completion-styles))
  (use-package! hotfuzz
    :init (setq completion-styles '(hotfuzz substring))))

;; Avy for colemak
(after! avy
  ;; home row priorities: 8 6 4 5 - - 1 2 3 7
  (setq avy-keys '(?n ?e ?i ?s ?t ?r ?i ?a)))

;; Auto-select if fewer than n completion candidates. Not used by company, it seems.
(setq completion-cycle-threshold 6)
(after! company
  ;; Tab and go setup. Uses a company frontend that cycles full completions with
  ;; the tab key, same as vim.
  (setq company-idle-delay 0
        company-selection-wrap-around t
        company-minimum-prefix-length 1)
  (setq company-dabbrev-other-buffers 1)

  ;; (setq company-posframe-font doom-font)
  (setq company-frontends '(company-tng-frontend company-box-frontend))
  ;; recommended for company-fuzzy
  (setq
        company-require-match nil            ; Don't require match, so you can still move your cursor as expected.
        company-tooltip-align-annotations t  ; Align annotation to the right side.
        company-eclim-auto-save nil          ; Stop eclim auto save.
        company-dabbrev-downcase nil)        ; No downcase when completion.
  ;; Enable downcase only when completing the completion. Reccomended for fuzzy
  (defun jcs--company-complete-selection--advice-around (fn)
    "Advice execute around `company-complete-selection' command."
    (let ((company-dabbrev-downcase t))
      (call-interactively fn)))
  (advice-add 'company-complete-selection :around #'jcs--company-complete-selection--advice-around)
  ;; (add-hook! company-fuzzy-mode-hook
  ;;   (add-to-list 'company-fuzzy-history-backends 'company-yasnippet))
  ;; (setq company-fuzzy-passthrough-backends '(company-yasnippet)

  ;; (setq company-show-quick-access 'left)
  (add-hook! 'evil-normal-state-entry-hook #'company-abort)
  (setq company-fuzzy-sorting-backend 'liquidmetal)

  ;; Prioritize prefix matches, then do fuzzy
  (setq company-fuzzy-prefix-on-top t)
  (setq company-fuzzy-show-annotation 1)
  (company-statistics-mode)
  ;; (company-flx-mode +1)

  (add-to-list 'hippie-expand-try-functions-list (lambda (arg) (call-interactively 'company-complete)))
  ;; lsp-mode will override this
  (set-company-backend! 'prog-mode
    '(:separate company-capf company-dabbrev company-yasnippet))
  (set-company-backend! 'text-mode
    ;; '(:separate company-capf company-dabbrev company-ispell company-yasnippet))
    '(:separate company-capf company-dabbrev company-yasnippet))
  ;; (add-hook! 'lsp-configure-hook
  ;;   (add-to-list 'lsp-company-backends 'company-dabbrev))

  (defun my/company-init-backends-h--advice-after ()
    "Reset company-fuzzy after company backends modified"
    (company-fuzzy-mode 1)
    (setq-local company-backends '(company-fuzzy-all-other-backends)))
  (advice-add '+company-init-backends-h :after #'my/company-init-backends-h--advice-after)
  (advice-add '+lsp-init-company-backends-h :after #'my/company-init-backends-h--advice-after))

;; make aborting less annoying.
;; Accept when certain characters entered.
;; (setq company-auto-commit t)
; Floating completion pop-up for company, with icons and documentation!
; Seems to break tng
;; (add-hook! 'company-mode-hook #'company-box-mode)
(after! company-bibtex
  (add-to-list 'company-backends 'company-bibtex))
;; Consider using instead. Prepends, rather than appends.
;; (set-company-backend! '(latex-mode, org-mode) 'company-bibtex)

;; Tab cycles through minibuffer completion options.
(setq minibuffer-complete-cycle t)

(setq-default history-length 1000)
(setq-default prescient-history-length 1000)
(setq which-key-idle-delay 0.5) ;; Which-key kicks in faster

(after! corfu
  (setq corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (setq corfu-auto t)                 ;; Enable auto completion
  (setq corfu-scroll-margin 5)        ;; Use scroll margin
  (setq corfu-preselect-first nil) ;; Disable candidate preselection (for TNG)
  (setq corfu-separator ?\s)          ;; Orderless field separator
  (setq corfu-quit-no-match 'separator) ;; Exit autocomplete on space if no match
  (global-corfu-mode)

  ;; (add-hook! 'eshell-mode-hook
  ;;   (setq-local corfu-auto nil))
  (add-hook! 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer))
(defun corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer if `completion-at-point' is bound."
  (when (where-is-internal #'completion-at-point (list (current-local-map)))
    ;; (setq-local corfu-auto nil) Enable/disable auto completion
    (corfu-mode 1)))
(defun corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input))
    ;; (setq-local corfu-auto nil) Enable/disable auto completion
    (corfu-mode 1)))

(after! cape
   (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-hook! text-mode-hook
    (add-to-list 'completion-at-point-functions #'cape-tex)
    (add-to-list 'completion-at-point-functions #'cape-ispell)
    (add-to-list 'completion-at-point-functions #'cape-dict))
  )

;; (setq hippie-expand-try-functions-list
;;       '(
;;         try-expand-dabbrev
;;         try-expand-dabbrev-all-buffers
;;         ;; try-expand-dabbrev-from-kill
;;         try-complete-lisp-symbol-partially
;;         try-complete-lisp-symbol
;;         try-complete-file-name-partially
;;         try-complete-file-name
;;         ;; try-expand-all-abbrevs
;;         ;; try-expand-list
;;         ;; try-expand-line
;;         ))

(use-package! spray
  :commands spray-mode
  :config
  (setq spray-wpm 500
        spray-height 800)
  (defun spray-mode-hide-cursor ()
    "Hide or unhide the cursor as is appropriate."
    (if spray-mode
        (setq-local spray--last-evil-cursor-state evil-normal-state-cursor
                    evil-normal-state-cursor '(nil))
      (setq-local evil-normal-state-cursor spray--last-evil-cursor-state)))
  (add-hook! 'spray-mode-hook #'spray-mode-hide-cursor)
  (map! :map spray-mode-map
        "<return>" #'spray-start/stop
        "j" #'spray-faster
        "k" #'spray-slower
        "t" #'spray-time
        "w" #'spray-forward-word
        "h" #'spray-forward-word
        "b" #'spray-backward-word
        "l" #'spray-backward-word
        "q" #'spray-quit))

;; make AUCTeX aware of style files and multi-file documents right away
(setq TeX-auto-save t) ; Parse on save
(setq TeX-parse-self t) ; Parse on load
(setq TeX-command-extra-options "-output-directory=./latexbuild")
(setq-default TeX-master nil)
(after! latex
(setf (nth 1 (assoc "LaTeX" TeX-command-list))
      "%`%l –output-directory=latexbuild -interaction=nonstopmode -outdir=latexbuild %(mode)%' %t"))
;;       max_print_line=2000 latexmk -verbose -file-line-error -synctex=1
;;       -interaction=nonstopmode -pdf -outdir=latexbuild -pvc -e

(if (eq system-type 'windows-nt)
    ;; WORKAROUND https://github.com/magit/magit/issues/2395
    (define-derived-mode magit-staging-mode magit-status-mode "Magit staging"
      "Mode for showing staged and unstaged changes."
      :group 'magit-status)
  (defun magit-staging-refresh-buffer ()
    (magit-insert-section (status)
      (magit-insert-untracked-files)
      (magit-insert-unstaged-changes)
      (magit-insert-staged-changes)))
  (defun magit-staging ()
    (interactive)
    (magit-mode-setup #'magit-staging-mode)))

;; If creating commit with nothing staged, auto-stage current file.
(setq magit-commit-ask-to-stage "stage")
(setq git-commit-major-mode 'markdown-mode)

;; ;; Permenantly show workspace list in minibuffer line
;; (after! persp-mode
;;   (defun display-workspaces-in-minibuffer ()
;;     (with-current-buffer " *Minibuf-0*"
;;       (erase-buffer)
;;       (insert (+workspace--tabline))))
;;   (run-with-idle-timer 1 t #'display-workspaces-in-minibuffer)
;;   (+workspace/display))


;; (setq LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))
;;     (setq auctex-latexmk-inherit-TeX-PDF-mode t)

;; (setq +latex-viewers '(pdf-tools))
;;
;; Undo the helm text enlargement in childframes
(setq +helm-posframe-text-scale 0)

;; This supresses the output window for async commands.
(defun async-shell-command-no-window
    (command)
  (interactive)
  (let
      ((display-buffer-alist
        (list
         (cons
          "\\*Async Shell Command\\*.*"
          (cons #'display-buffer-no-window nil)))))
    (async-shell-command
     command)))

(use-package! pkgbuild-mode
  :mode "\\PKGBUILD")
  (use-package! vimrc-mode
  :mode "\\.vimrc\\'")
  (use-package! systemd
  :mode "\\.service\\'")
  (use-package! sxhkd-mode
  :mode "\\sxhkdrc\\'")
(use-package! dockerfile-mode
  :mode "Dockerfile\\'"
  :config
  (put 'dockerfile-image-name 'safe-local-variable #'stringp)
  )

;; For terminal to have shape-changing cursors.
(global-term-cursor-mode)


;; IDK if this is even real
(after! dired-k
        (setq dired-k-human-readable t))
(use-package! poly-markdown
  :after markdown)
(use-package! poly-org
  :after org)

(setq backward-delete-char-untabify-method 'hungry)

(define-minor-mode visual-line-motion-mode
  "Toggle visual-line-motion-mode
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When visual-line-motion-mode is enabled, evil motions operate on visual lines
rather than file lines."
  :init-value nil
  :global nil
  ;; evil respects visual-line-mode, but not truncate-lines. In other words, enabling visual-line-mode (aka word-wrap-mode) will enable visual line motions (j=gj)
  ;; Maybe eventually swap out doom's word-wrap-mode for plain visual-line-mode or something else.
  (if visual-line-motion-mode
      (progn
        ;; (toggle-truncate-lines 0)
        ;; (+word-wrap-mode t)
        (visual-line-mode t)
        )
    (progn
      ;; (toggle-truncate-lines t)
      ;; (+word-wrap-mode nil)
      (visual-line-mode nil)
      ))
  )

(after! dap-mode
  (require 'dap-python)
  )

(add-hook! 'realgud-short-key-mode-hook
          (map! :n :leader "d" realgud:shortkey-mode-map))


(after! vterm
  ;; (set-popup-rule! "\*doom:vterm-popup:doom\*" :side 'bottom :size 0.2)
  (set-popup-rule! "^\\*doom:\\(?:v?term\\|e?shell\\)-popup" :side 'bottom :size 0.2)
  (set-popup-rule! "^\\*doom:vterm" :side 'bottom :size 0.2)
  )
  (set-popup-rule! "^\\*doom:\\(?:v?term\\|e?shell\\)-popup" :side 'bottom :size 0.2)
  (set-popup-rule! "^\\*doom:vterm" :side 'bottom :size 0.2)


(after! evil-extra-operator
  (global-evil-extra-operator-mode 1)
  (dolist (new (list
                '(ess-r-mode ess-eval-region)))
    (add-to-list 'evil-extra-operator-eval-modes-alist new)))

(after! wc-mode
  (setq wc-modeline-format "WC[%tw:%ktc]")
  (setq wc-idle-wait 2))

;; Put ess stuff in modeline
(add-hook! 'inferior-ess-mode-hook
        (add-to-list 'mode-line-process '(:eval (nth ess--busy-count ess-busy-strings))))

;; Prevent these errors freezing the whole editor
(advice-add 'ispell-lookup-words :around
            (lambda (orig &rest args)
              (shut-up (apply orig args))))
