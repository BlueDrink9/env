;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

; TODO: add git gutter, replacement operator, ...

;; Run '~/.emacs.d/bin/doom sync' on the command line, then restart Emacs. 
;; Or use 'M-x doom/reload'.
(unpin! evil-collection)
;; Disables a default 'jk' normal state binding. key-chord replaces it more
;; flexibly anyway.
(package! evil-escape :disable t)

(package! key-chord)
;; Allows rebinding whole prefix trees
(package! rebinder :recipe (:host github :repo "darkstego/rebinder.el"))

;; These are from the official snippets repo.
(package! yasnippet-snippets
  :recipe (:host github
           :repo "AndreaCrotti/yasnippet-snippets"
           :files ("*")))

;; (package! evil-collection) ;; included already.
(package! evil-quickscope)
;; String modification functions
(package! s)

(package! realgud)
(package! dap-mode)

;; Tab bar customisation
(package! awesome-tab)
;; Tab key behavior
;; (package! smart-tab)
(package! smart-tab :recipe (:host git.genehack.net :repo "genehack/smart-tab" :files ("*.el")))

;; Autosaving
(package! super-save)
(package! auctex)

;; Completions
;; (package! counsel)

;; Coloured man pages
(package! info-colors)

;; Spritz-like speed reading.
(package! spray)

;; Load huge files in chunks. Lazy-load so disable initially.
(package! vlf :recipe (:host github :repo "m00natic/vlfi" :files ("*.el")) :disable t)


;; (package! org-pretty-table
;;   :recipe (:host github :repo "Fuco1/org-pretty-table"))
;; (package! graphviz-dot-mode)
;; Hide markup symbols unless in them.
(package! org-appear
  :recipe (:host github
           :repo "awth13/org-appear"))

;; Hook into calibre and read ebooks
;; (package! calibredb)
;; (package! nov)
;; https://tecosaur.github.io/emacs-config/config.html#ebooks

(package! smartparens)

; Floating completion pop-up for company, with icons and documentation!
(package! company-box)
(package! all-the-icons-dired)

(package! mixed-pitch)
;; (package! bufler)

(package! dockerfile-mode)
(package! pkgbuild-mode
  :recipe (:host github
            :repo "juergenhoetzel/pkgbuild-mode"))
(package! vimrc-mode)
(package! systemd)
(package! sxhkd-mode
  :recipe (:host github
            :repo "ymarco/sxhkd-mode"))

;; Combine R and markdown mode in one buffer with polymode
;; For working with .Rmd files and better orgmode R support.
(package! polymode)
(package! poly-R)
(package! poly-org)
(package! poly-markdown)


;; (package! evil-better-visual-line)

(package! term-cursor :recipe (:host github :repo "h0d/term-cursor.el" :files ("*.el")))

(package! minibuffer-complete-cycle)

(package! forge)
;; Consider disabling these packages since they are heavier.
;; =:ui tabs=, =:ui indent-guides=, =:ui ligatures=, =:editor word-wrap= and =:ui

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))
;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
