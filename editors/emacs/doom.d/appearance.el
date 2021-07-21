;;; $DOOMDIR/appearance.el -*- lexical-binding: t; -*-
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

(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))


;; Change window title
;; (setq frame-title-format
;;       '(""
;;         (:eval
;;          (if (s-contains-p org-roam-directory (or buffer-file-name ""))
;;              (replace-regexp-in-string
;;               ".*/[0-9]*-?" "☰ "
;;               (subst-char-in-string ?_ ?  buffer-file-name))
;;            "%b"))
;;         (:eval
;;          (let ((project-name (projectile-project-name)))
;;            (unless (string= "-" project-name)
;;              (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
(setq-default word-wrap t)
(add-hook 'linum-mode (lambda () (face-remap-add-relative 'default '(:family "Monospace"))))
;; https://stackoverflow.com/questions/9304192/emacs-linum-mode-and-size-of-font-unreadable-line-numbers
(eval-after-load "linum"
  '(set-face-attribute 'linum nil 'default '(:family "Monospace")))


;; Custom tab appearance
(after! centaur-tabs
  (centaur-tabs-mode -1)
  (setq centaur-tabs-height 26
        centaur-tabs-set-icons t
        centaur-tabs-modified-marker "o"
        centaur-tabs-close-button "×"
        centaur-tabs-set-bar 'above
        centaur-tabs-gray-out-icons 'buffer)
  )
;; (setq x-underline-at-descent-line t)