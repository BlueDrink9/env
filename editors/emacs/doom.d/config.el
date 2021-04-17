;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
; TODO: insert single char; ex command; unimparied options;
(setq script_dir (file-name-directory (or load-file-name buffer-file-name)))
(load-file (concat script_dir "bindings.el"))
(load-file (concat script_dir "aliases.el"))
(load-file (concat script_dir "file-mode-settings.el"))
(use-package! s)
(load-file (concat script_dir "spellcheck.el"))
;; Remember, you do not need to run 'doom sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
; (setq user-full-name "John Doe"
;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-font (font-spec :family "SauceCodePro NF" :size 16))
(setq doom-variable-pitch-font (font-spec :family "Source Sans Pro" :size 16))

;; ;; There are two ways to load a theme. Both assume the theme is installed and
;; ;; available. You can either set `doom-theme' or manually load a theme with the
;; ;; `load-theme' function. This is the default:
;; Trying to set based on colourscheme. Broken, for strange variable typing reasons I think.
;; (
;;  let ((colourscheme (getenv "COLOURSCHEME")))
;;  (if (or (not colourscheme) (equal "" colourscheme))
;;      (setq doom-theme 'doom-one)
;;    (setq colourscheme (s-replace "light" "-light" colourscheme))
;;    (setq colourscheme (s-replace "dark" "-dark" colourscheme))
;;    (setq colourscheme (s-replace "_" "-" colourscheme))
;;    ;; One theme specifies doom-one-light, but dark theme does not.
;;    (if (string-match-p (regexp-quote "one") colourscheme)
;;        (setq colourscheme (s-replace "-dark" "" colourscheme)))
;;    (setq colourscheme (concat "doom-" colourscheme))
;;    (message "''%s''" colourscheme)
;;    (setq doom-theme colourscheme)
;;    )
;;  )
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/work/org/")

(setq evil-collection-setup-minibuffer t)
(setq evil-want-minibuffer t)
(add-hook 'minibuffer-setup-hook
      (lambda () (setq truncate-lines nil)))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
(setq-default word-wrap t)
(add-hook 'linum-mode (lambda () (face-remap-add-relative 'default '(:family "Monospace"))))
;; https://stackoverflow.com/questions/9304192/emacs-linum-mode-and-size-of-font-unreadable-line-numbers
(eval-after-load "linum"
  '(set-face-attribute 'linum nil 'default '(:family "Monospace")))

;; Use " register by default, instead of system. System register is still
;; accessible via evil + and * registers.
;; Equivalent of vim `set clipboard=unnamed`.
(setq select-enable-clipboard nil)

(scroll-bar-mode t)
(set-scroll-bar-mode 'right)
;; Temporary, but will be useful for learning my way around.
(menu-bar-mode t)

;; Doom disables auto-save/backup by default.
(setq auto-save-default t
      make-backup-files t)

;; Disable smartparens. Can't be done another way because it is a default package.
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;; Fancy bullets in org mode, heavy plugin.
(remove-hook 'org-mode-hook #'org-superstar-mode)

   ;; (after! tabbar
   ;;   (setq org-fontify-quote-and-verse-blocks nil
   ;;         org-startup-indented nil))
(after! evil-snipe
  (evil-snipe-mode -1))
;; (after! evil-snipe
;;   (evil-snipe-override-mode nil))
;; Use cx for exchange mapping
(after! evil-exchange
  (evil-exchange-cx-install))
;; Only highlight targets when f,F,T,t pressed.
(after! evil-quickscope
  (global-evil-quickscope-mode 1)
  )


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
