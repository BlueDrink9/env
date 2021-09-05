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

(defun my/font-exists (font)
  "check if font exists"
  (if (null (x-list-fonts font)) nil t))

(setq my/fonts '("SauceCodePro NF" "Source Code Pro"))
(setq my/proportional-fonts '("Source Sans Pro" "Consolas" "Ariel"))

(when (display-graphic-p)
  (catch 'done
    (dolist (font my/fonts)
      (if (my/font-exists font) (progn
                                  (setq doom-font (font-spec :family font :size 16))
                                  (throw 'done nil)
                                  ))))


  (catch 'done
    (dolist (font my/proportional-fonts)
      (if (my/font-exists font) (progn
                                  (setq doom-variable-pitch-font (font-spec :family font :size 16))
                                  (throw 'done nil)
                                  ))))
  )

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


(defun my/title-format ()
  '(:eval
    (format "%s %s"
            (buffer-name)
            (cond
             (buffer-file-truename
              (concat "(" buffer-file-truename ")"))
             (dired-directory
              (concat "{" dired-directory "}"))
             (t
              "")))
  ))
(setq frame-title-format
      (setq icon-title-format
            (my/title-format)
            ))

(define-globalized-minor-mode my/global-lsp-headerline-breadcrumb-mode lsp-headerline-breadcrumb-mode
  (lambda () (lsp-headerline-breadcrumb-mode 1)))
(after! lsp-mode
  (my/global-lsp-headerline-breadcrumb-mode 1)
  )
;; (add-hook! 'text-mode-hook
;;   (lsp-headerline-breadcrumb-mode 0)
;;   )
;; Can also include project, file, and path if you want to.
(setq lsp-headerline-breadcrumb-segments '(symbols))

(scroll-bar-mode t)
(set-scroll-bar-mode 'right)
;; Temporary, but will be useful for learning my way around.
(if (display-graphic-p)
    (menu-bar-mode t)
    (menu-bar-mode 0)
  )
