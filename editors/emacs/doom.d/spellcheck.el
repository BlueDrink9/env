;;; spellcheck.el -*- lexical-binding: t; -*-
;;; Modified to change dict to en_AU, and to change `ispell-local-dictionary` to
;;; `ispell-dictionary` everywhere.  Also to use ispell-dictionary as variable
;;; everywhere. May fail and need changing to local-dictionary if you change
;;; dictionary on the fly.
;; (setq ispell-aspell-data-dir "/nix/store/my1hch4x12i5n1ffhlwpb6bpsiajbv3j-aspell-0.60.6.1/lib/aspell/")
(setq ispell-aspell-dict-dir ispell-aspell-data-dir)
(setq ispell-dictionary "en_AU")
;; Spell check config for programmers.
;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
;; if (aspell installed) { use aspell}
;; else if (hunspell installed) { use hunspell }
;; whatever spell checker I use, I always use English dictionary
;; I prefer use aspell because:
;; 1. aspell is older
;; 2. looks Kevin Atkinson still get some road map for aspell:
;; @see http://lists.gnu.org/archive/html/aspell-announce/2011-09/msg00000.html
(defun flyspell-detect-ispell-args (&optional run-together)
  "if RUN-TOGETHER is true, spell check the CamelCase words."
  (let (args)
    (cond
     ((string-match  "aspell$" ispell-program-name)
      ;; Force the English dictionary for aspell
      ;; Support Camel Case spelling check (tested with aspell 0.6)
      (setq args (list "--sug-mode=ultra" (concat "--lang=" ispell-dictionary)))
      (when run-together
        (cond
         ;; Kevin Atkinson said now aspell supports camel case directly
         ;; https://github.com/redguardtoo/emacs.d/issues/796
         ((string-match-p "--camel-case"
                          (shell-command-to-string (concat ispell-program-name " --help")))
          (setq args (append args '("--camel-case"))))

         ;; old aspell uses "--run-together". Please note we are not dependent on this option
         ;; to check camel case word. wucuo is the final solution. This aspell options is just
         ;; some extra check to speed up the whole process.
         (t
          (setq args (append args '("--run-together" "--run-together-limit=16")))))))
     ((string-match "hunspell$" ispell-program-name)
      ;; Force the English dictionary for hunspell
      (setq args(concat "-d" ispell-dictionary))))
    args))

(cond
 ((executable-find "aspell")
  ;; you may also need `ispell-extra-args'
  (setq ispell-program-name "aspell"))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")

  ;; Please note that `ispell-dictionary` itself will be passed to hunspell cli with "-d"
  ;; it's also used as the key to lookup ispell-dictionary-alist
  ;; if we use different dictionary
  (setq ispell-dictionary-alist
        '((ispell-dictionary "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" (ispell-dictionary)) nil utf-8))))
 (t (setq ispell-program-name nil)))

;; ispell-cmd-args is useless, it's the list of *extra* arguments we will append to the ispell process when "ispell-word" is called.
;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
;; Please note when you use hunspell, ispell-extra-args will NOT be used.
;; Hack ispell-local-dictionary-alist instead.
(setq-default ispell-extra-args (flyspell-detect-ispell-args t))
;; (setq ispell-cmd-args (flyspell-detect-ispell-args))
(defadvice ispell-word (around my-ispell-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)))

(defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use emacs original arguments
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)))

(defun text-mode-hook-setup ()
  ;; Turn off RUN-TOGETHER option when spell check text-mode
  (setq-local ispell-extra-args (flyspell-detect-ispell-args)))
(add-hook 'text-mode-hook 'text-mode-hook-setup)
