;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

; TODO: add git gutter, replacement operator, ...

;; Run '~/.emacs.d/bin/doom sync' on the command line, then restart Emacs. 
;; Or use 'M-x doom/reload'.

(package! key-chord)
;; (package! evil-collection) ;; included already.
(package! evil-quickscope)
;; String modification functions
(package! s)

(package! realgud)
(package! dap-mode)

;; Tab bar customisation
(package! awesome-tab)
;; Tab key behavior
(package! smart-tab)

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

;; Hook into calibre and read ebooks
;; (package! calibredb)
;; (package! nov)
;; https://tecosaur.github.io/emacs-config/config.html#ebooks

;; (package! smartparens :disable t)

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
