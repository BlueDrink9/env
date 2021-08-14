;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
; TODO: insert single char; ex command; unimparied options;
; TODO: autobracket
; TODO: jumping to mark should put mark in middle of view
; TODO: scrolloff
; TODO: viminfo-style savings (persistend jump list, registers)
; TODO: Bacsspace undoes tab

(setq script_dir (file-name-directory (or load-file-name buffer-file-name)))
(load-file (concat script_dir "bindings.el"))
(load-file (concat script_dir "aliases.el"))
(load-file (concat script_dir "appearance.el"))
(load-file (concat script_dir "file-mode-settings.el"))
(use-package! s)
(load-file (concat script_dir "spellcheck.el"))
;; Remember, you do not need to run 'doom sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
; (setq user-full-name "John Doe"
;       user-mail-address "john@doe.com")


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/work/org/")

;; Uses # for comments, generally a good fit for files without a specified file extension.
(setq-default major-mode 'conf-mode)

;; Use " register by default, instead of system. System register is still
;; accessible via evil + and * registers.
;; Equivalent of vim `set clipboard=unnamed`.
(setq select-enable-clipboard nil)

(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(scroll-bar-mode t)
(set-scroll-bar-mode 'right)
;; Temporary, but will be useful for learning my way around.
(menu-bar-mode t)

;; Doom disables auto-save/backup by default.
(setq auto-save-default t
      make-backup-files t)
(setq scroll-margin 2)
;; Autosaves
(super-save-mode +1)
(after! super-save
  (setq super-save-remote-files nil)
  ;; (setq super-save-exclude '(".gpg"))
  (add-to-list 'super-save-hook-triggers 'evil-insert-state-exit-hook))


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

;; Disable smartparens. Can't be done another way because it is a default package.
;; (remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
;; or: from an old github issue:
;; (after! smartparens (smartparens-global-mode -1))
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

;; Fancy bullets in org mode, heavy plugin.
(remove-hook 'org-mode-hook #'org-superstar-mode)

   ;; (after! tabbar
   ;;   (setq org-fontify-quote-and-verse-blocks nil
   ;;         org-startup-indented nil))
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

;; (after! org
;;   (setq org-fontify-quote-and-verse-blocks nil
;;         org-fontify-whole-heading-line nil
;;         org-hide-leading-stars nil
;;         org-startup-indented nil))

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

;; Lazy-load
(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

;; (use-package! org-pretty-table
;;   :commands (org-pretty-table-mode global-org-pretty-table-mode))

;; Avy for colemak
(after! avy
  ;; home row priorities: 8 6 4 5 - - 1 2 3 7
  (setq avy-keys '(?n ?e ?i ?s ?t ?r ?i ?a)))

;; More frequent completions
(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (setq company-show-quick-access t)
  (add-hook! 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.

; Floating completion pop-up for company, with icons and documentation!
(add-hook! company-mode company-box-mode)

(setq-default history-length 1000)
(setq-default prescient-history-length 1000)


(setq which-key-idle-delay 0.5) ;; Which-key kicks in faster


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
(setq-default TeX-master nil)
(setf (nth 1 (assoc "LaTeX" TeX-command-list))
      "%`%l –output-directory=latexbuild -interaction=nonstopmode -outdir=latexbuild %(mode)%' %t")
;;       max_print_line=2000 latexmk -verbose -file-line-error -synctex=1
;;       -interaction=nonstopmode -pdf -outdir=latexbuild -pvc -e

;; If creating commit with nothing staged, auto-stage current file.
(setq magit-commit-ask-to-stage "stage")

;; ;; Permenantly show workspace list in minibuffer line
;; (after! persp-mode
;;   (defun display-workspaces-in-minibuffer ()
;;     (with-current-buffer " *Minibuf-0*"
;;       (erase-buffer)
;;       (insert (+workspace--tabline))))
;;   (run-with-idle-timer 1 t #'display-workspaces-in-minibuffer)
;;   (+workspace/display))

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

;; For terminal (at least, iterm)
(after! term-cursor
  (global-term-cursor-mode))


;; IDK if this is even real
(after! dired-k
        (setq dired-k-human-readable t))
;; IDK if this is even real
(setq +ivy-project-search-engines '(rg))

(after! markdown
  (use-package! poly-markdown))
(after! org
  (use-package! poly-org))

;; Hides markup symbols until you enter the word
(add-hook! org-mode :append #'org-appear-mode)
