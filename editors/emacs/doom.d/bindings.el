;;; mappings.el -*- lexical-binding: t; -*-

;; It is important keys are bound after the keymap is loaded.  For local keys,
;; include an `:after` entry for the package that defines that mode, then
;; specify the mode `:map`.
;; For mapping evil-local commands, ensure state (:nvio) is specified after
;; `:after` and `:map`.
;; Eg: Map 'r' in normal mode to repeat pytest.
;; (map! :after python
;;       :map python-mode-map
;;       :nv
;;       "r" #'python-pytest-repeat)

;; Unbound leader keys: d j k l

;; ;; Overridden in some buffers, eg DIRED
;; (setq doom-localleader-key "<return>")
;; (setq doom-localleader-key "\\")
;; Leaving spc m (the default) for now.

;; Requires KeyChord library
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.3
      key-chord-safety-interval-forward 0.05)
(map! :desc "Enter normal mode"
      :i (general-chord "kv") 'evil-normal-state
      :i (general-chord "vk") 'evil-normal-state)

(require 'rebinder)
(rebinder-hook-to-mode 'evil-normal-state-map 'evil-normal-state-entry-hook)
(rebinder-hook-to-mode 'evil-visual-state-map 'evil-visual-state-entry-hook)
;; Replace prefixes C-x and leader c
;; (map! :n :leader "C-x" (rebinder-dynamic-binding "C-x"))
;; For some reason using :prefix causes an error with this function.
(map! :desc "C-x" :leader "z" (rebinder-dynamic-binding "C-x"))
(map! :desc "C-c" :nv "S-SPC" (rebinder-dynamic-binding "C-c"))
(map! :desc "C-c" :leader "v" "C-c")
(map! :desc "C-k" :leader "<backspace>" "C-c")
;; (map! :desc "C-x" :nv "SPC z" #'Control-X-prefix)
;; (map! :desc "C-c" :nv "S-SPC" #'mode-specific-command-prefix)
;; (map! :desc "C-c" :nv "S-SPC" mode-specific-map)
;; (map! :desc "God-mode C-c" :nv "S-SPC" #'evil-execute-in-god-state)
(map! :map evil-god-state-map "<escape>" 'evil-god-state-bail)
(which-key-add-key-based-replacements "SPC z" "C-x")
(which-key-add-key-based-replacements "S-SPC" "C-c")
;; (map! :desc "C-c" :nv "S-SPC" (setq unread-command-events
;;            (listify-key-sequence (kbd "C-c"))))

;;;###autoload
(defun my/get-current-C-c-map ()
  (interactive)
  "Keymap for both global and current local C-c prefix bindings"
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map
        (make-composed-keymap mode-specific-map
                              (lookup-key (current-local-map) (kbd "C-c"))))
    map))
(map! :desc "C-c" :nv "S-SPC" #'my/get-current-C-c-map)


;;;###autoload
(defun my/evil-collection-translations (_mode mode-keymaps &rest _rest)
  (evil-collection-translate-key 'normal mode-keymaps
    ;; these need to be unbound first; this needs to be in same statement
    (kbd "C-j") nil
    (kbd "C-k") nil
    (kbd "M-j") (kbd "C-j")
    (kbd "M-k") (kbd "C-k")
    ))
(add-hook! 'evil-collection-setup-hook #'my/evil-collection-translations)


;; Swap ;, :
(map! :nv ";" #'evil-ex
      :nv ":" #'evil-repeat-find-char)
      ;; Snipe gives colour hints.
      ;; :nv ":" #'evil-snipe-repeat)
;; (evil-collection-swap-key nil 'evil-motion-state-map ";" ":")
(map! :leader :nv
      ";" #'execute-extended-command  ;; Vertico
      :nv ":" #'pp-eval-expression)


;; Don't allow any package to override these keymaps.
(map! :map 'override
      :nv  "C-l" #'evil-window-right
      :nv  "C-h" #'evil-window-left
      :nv  "C-k" #'evil-window-up
      :nv  "C-j" #'evil-window-down)

;; And yet, some insist on it.
(after! ranger
  (map! :map ranger-normal-mode-map
        "C-l" #'evil-window-right
        "C-h" #'evil-window-left
        "C-k" #'evil-window-up
        "C-j" #'evil-window-down))


;; (map! :after outline
;;       :map outline-mode-map
;;       <C-j> nil
;;       <C-k> nil)

(map! :desc "split window right" :nv  "C-w l" #'evil-window-vsplit)
(map! :desc "split window down" :nv  "C-w j" #'evil-window-split)

(map! :map evil-window-map
      "t"       #'+workspace/new
      "<"       #'centaur-tabs-move-current-tab-to-left
      ">"       #'centaur-tabs-move-current-tab-to-right
      "<home>"       #'centaur-tabs-select-beg-tab
      "<end>"       #'centaur-tabs-select-end-tab
      ;; Swapping windows. Defined by default anyway.
      ;; "S-<left>"       #'+evil/window-move-left
      ;; "S-<down>"       #'+evil/window-move-down
      ;; "S-<up>"         #'+evil/window-move-up
      ;; "S-<right>"      #'+evil/window-move-right
      )

(map! :n  "<right>" #'centaur-tabs-forward
      :n  "<left>" #'centaur-tabs-backward
      :n  "<C-tab>" #'centaur-tabs-forward
      :n  "<C-iso-lefttab>" #'centaur-tabs-backward)
;; Unsure if I want tabs or workspaces.
;; (map! :desc "buf-next" :nv  "<up>" #'tabnext)
;; (map! :desc "buf-next" :nv  "<down>" #'evil-ex "tabprev")
(map! :n  "<up>"      #'+workspace/switch-right
      :n  "<down>"    #'+workspace/switch-left
)
(map! :map evil-window-map
      "["    #'persp-prev
      "]"    #'persp-next
      "S-["    #'+workspace/swap-left
      "S-]"    #'+workspace/swap-right
      )

(map! :map 'override :n
      "S-<left>"       #'(lambda () (interactive) (evil-window-increase-width 5))
      "S-<right>"      #'(lambda () (interactive) (evil-window-increase-width 5))
      "S-<down>"       #'(lambda () (interactive) (evil-window-increase-height 5))
      "S-<up>"         #'(lambda () (interactive) (evil-window-increase-height 5))
      "C-<left>"       #'(lambda () (interactive) (evil-window-decrease-width 5))
      "C-<right>"      #'(lambda () (interactive) (evil-window-decrease-width 5))
      "C-<down>"       #'(lambda () (interactive) (evil-window-decrease-height 5))
      "C-<up>"         #'(lambda () (interactive) (evil-window-decrease-height 5))
      )



(map! :leader
      :desc "evilnc-comment-operator" :nv "C-/" #'evilnc-comment-operator
      :desc "evilnc-comment-operator" :nv "c" #'evilnc-comment-operator)
;; By default bound to leader-c
(map! :desc "code" :leader :nv "e" doom-leader-code-map)
(map! :map doom-leader-code-map :desc "LSP" "l" #'+default/lsp-command-map)

(map! :map doom-leader-code-map :m "e" 'evil-operator-eval)

(map! :desc "Describe key" :nv "C-?" #'describe-key-briefly)

;;;###autoload
(defun my/evil-paste-from-system ()
  "Paste from system register ('+') "
  (interactive)
  (evil-paste-from-register ?+))

;;;###autoload
(defun my/evil-use-system-register-next ()
  "Sets the register for the next action to the system register"
  (interactive)
  ;; (evil-use-register ?+))
  (setq evil-this-register ?+))
;; (map! :desc "system register" :nv (general-chord "\"\"") #'my/evil-use-system-register-next)

;; TODO: Make this not override the unnamed register
(map! :desc "Use system register next" :nv (general-chord "\"\"") "\"+")
;; Cannot have 3-key chords :(
;; (map! :desc "Use black hole register next" :nv (general-chord "\"\"\"") "\"_")
(map! :desc "Paste from system clipboard" :i "C-S-v" #'my/evil-paste-from-system
      :i "C-v" #'my/evil-paste-from-system)

;; maps for everywhere (all modes)
;; (with-eval-after-load 'evil-maps
  ;; (define-key evil-motion-state-map (kbd "C-?") 'describe-key-briefly)
  ;; )
;; maps for specific modes
;; (evil-define-key 'normal org-mode-map "\<" 'org-metaleft )

;; Don't add to register on x or X
(map! :desc "\"_x" :nv  "x" #'delete-forward-char
      :nv  "X" #'delete-backward-char)
(map! :desc "black hole register delete" :v  "<delete>" #'evil-delete-char)

;; (map! :desc "Toggle fold" :n  "<backspace>" #'+fold/toggle)
;; Use the keys (za) rather than the function so that it works with more
;; evilified minor modes (eg magit).
(map! :desc "Toggle fold" :n  "<backspace>" "za"
      :desc "Create fold from selection" :v  "<backspace>" #'evil-vimish-fold/create)

(map! :desc "Go to mark" :n  "'" #'evil-goto-mark
      :desc "Go to mark line" :n  "`" #'evil-goto-mark-line)

;; (map! :desc "Dot operator without moving" :n  "." #'evil-repeat | evil-goto-mark(".")) ; want to replicate .'.

;; Insertmode deletes create an undopoint first
;; inoremap <c-u> <c-g>u<c-u>

(map! :desc "Faster redo" :n  "U" #'evil-redo
      :desc "Run most recent macro" :n  "Q" #'evil-execute-macro)

(map! :leader :desc "Go to alternate file" :n  "a" #'evil-switch-to-windows-last-buffer)

;;  by default keeps C-x and c-a for emacs things.
(map! :desc "Increment number" :n  "C-a" #'evil-numbers/inc-at-pt)
      ;; :desc "Decrement number" :n  "C-x" #'evil-numbers/dec-at-pt)
(map! :map rebinder-mode-map "C-x" 'evil-numbers/dec-at-pt)
;; (defun my/c-x ()
;;   (interactive)
;;   (setq unread-command-events (listify-key-sequence (kbd "C-x"))))
;; (map! :leader :desc "ctrl x replacement" :nv "C-x" #'my/c-x)

(map! :leader :desc "undo-tree-visualise" :n "u" #'undo-tree-visualize
      :n "C-u" #'universal-argument)
;; No real equivalent?
;; (map! :n "g +" #'undo-tree-visualise-switch-branch-right
;;       :n "g -" #'undo-tree-visualize-switch-branch-left)

;; (map! :desc "CD to current file's dir" :n  "SPC c d" #')

; n and N always go the same direction regardless of whether / or ? was used.

;; ;; Autoexpand brackets when creating functions etc.
;;;###autoload
(defun autobracket (bracCharOpen bracCharClose)
  (interactive)
  (insert bracCharOpen)
  (evil-insert-newline-below)
  (insert bracCharClose)
  (evil-previous-line)
  (evil-insert-newline-below)
  (evil-indent-line))
;; ;; This mostly works but will not ever trigger properly because key-chord.el cannot bind return key!
;; (map! :desc "Autoexpand bracket" :i
;;       (general-chord (concat "(" (kbd "<RET>")))
;;       (lambda () (interactive) (autobracket "(" ")")))

(map! :desc "Autoexpand bracket" :i
      (general-chord "{;")
      (lambda () (interactive) (autobracket "{" "};")))
(map! :desc "Autoexpand bracket" :i
      (general-chord "{,")
      (lambda () (interactive) (autobracket "{" "},")))
(map! :desc "Autoexpand bracket" :i
      (general-chord "[;")
      (lambda () (interactive) (autobracket "[" "];")))
(map! :desc "Autoexpand bracket" :i
      (general-chord "[,")
      (lambda () (interactive) (autobracket "[" "],")))
;; inoremap (<CR> (<CR>)<Esc>O
;; inoremap {<CR> {<CR>}<Esc>O
;; inoremap {; {<CR>};<Esc>O
;; inoremap {, {<CR>},<Esc>O
;; inoremap [<CR> [<CR>]<Esc>O
;; inoremap [; [<CR>];<Esc>O
;; inoremap [, [<CR>],<Esc>O

(evil-define-command my/evil-insert-char (count char)
  (interactive "<c><C>")
  (setq count (or count 1))
  (insert (make-string count char)))
(map! :map 'override :desc "Insert a single char before cursor" :n "," #'my/evil-insert-char)

(map! :desc "Easymotion prefix" :nvo "s" nil)
(after! evil-easymotion
  (evilem-default-keybindings "s")
  (map! :nvo "s l" #'evilem-motion-forward-word-begin
        :nvo "s h" #'evilem-motion-backward-word-begin)
  )

;; Git/magit shortcuts
(map! (:map doom-leader-git-map
       :desc "pusH to origin" "h" #'magit-push-current-to-pushremote
       :desc "pull from origin" "p" #'magit-pull-from-pushremote
       :desc "Amend commit" :prefix "c" "a" #'magit-commit-amend
       :desc "Extend commit" :prefix "c" "e" #'magit-commit-extend
       ))
(evil-ex-define-cmd "gc" #'magic-commit-create)
(evil-ex-define-cmd "gw" #'magit-stage-file)
(evil-ex-define-cmd "gup" #'magit-commit-extend)

;; TODO Magit: Allow w navigations, easymotion? For yanking commit diffs.
;; Add space mapping to cancel commit
(map! :after git-commit
      :map git-commit-mode-map
      :desc "confirm commit message" "C-<return>" 'with-editor-finish
      :desc "cancel commit" :leader "<backspace>" 'with-editor-cancel)
(map! :after magit
      :map magit-status-mode-map
      "J" #'magit-section-forward
      "K" #'magit-section-backward
      ":" #'magit-git
      "?" #'evil-search-backward
      )

;; (map! :leader
;;       (:prefix-map ("a" . "applications")
;;        (:prefix ("j" . "journal")
;;         :desc "New journal entry" "j" #'org-journal-new-entry
;;         :desc "Search journal entry" "s" #'org-journal-search)))

;; ;; Apparently have to add mapping to every file mode?
;; Getting overridden by company for some reason.
;; (after! smart-tab
;;   (dolist (tab (list "TAB" "<tab>"))
;;     (map! :i tab 'smart-tab)
;;     (map! :map smart-tab-mode-map tab 'nil)
;;     (add-hook! 'find-file-hook (map! :i tab 'smart-tab))))

;; (dolist (tab (list "TAB" "<tab>"))
;;   (map! :i tab #'hippie-expand))
(map! :i "C-/" #'hippie-expand)


(map! :map (vertico-map)
      :in "<escape>" #'minibuffer-keyboard-quit
      ;; Useful in normal mode sometimes if I want to use j/k to navigate.
      :ni "C-e" #'vertico-insert
      ;; :n "J" #'vertico-next-history-element
      ;; :n "K" #'vertico-previous-history-element
      :in "TAB" #'vertico-next
      :in "<backtab>" #'vertico-previous
      )

;; ;; Eventually will want to change this to something that first tries local buffer expansion, I think.
(after! company
  (defun my/transition-to-evil-normal-after-tng-chord ()
    "Transition to evil-normal-state after company-tng completion when a key-chord is detected."
    (when unread-command-events
      (if (eq (car unread-command-events) 'key-chord)
          (progn
            (company-complete)
            (evil-normal-state)
                                        ; Clear the tng event storage that thinks it needs to trigger an extra key-chord
            (setq unread-command-events nil)
                                        ; Reset key-chord's unmatched key state - not sure this is actually necessary
            (setq key-chord-last-unmatched nil)))))

  (advice-add 'company--unread-this-command-keys :after #'my/transition-to-evil-normal-after-tng-chord)


  ;; (map! :i "C-e" #'company-complete-common)
  (map! (:map company-active-map
              "C-e" #'company-complete-selection
              "C-n" #'evil-complete-next
              "C-p" #'evil-complete-previous
              "TAB" #'company-select-next
              ;; Shfit tab
              "<C-iso-lefttab>" #'company-select-previous
              ;; "C-/" #'counsel-company  ; search results
              "RET" #'newline-and-indent
              "<return>" #'newline-and-indent
              )))

(after! corfu
  (map! :i "C-e" #'corfu-complete)
  (map! (:map corfu-map
              [evil-escape] #'corfu-quit
              "TAB" #'corfu-next
              "C-n" #'corfu-next
              "C-p" #'corfu-previous
              "RET" #'newline-and-indent
              "<return>" #'newline-and-indent
              "S-TAB" #'corfu-previous)))

;; Minibuffer only one escape to exit, kj to navigate history.
(map! :map (minibuffer-local-map evil-ex-completion-map evil-ex-search-keymap)
      :in "<escape>" #'abort-recursive-edit
      :in "C-c" #'abort-recursive-edit
      :n "j" #'next-complete-history-element
      :n "k" #'previous-complete-history-element
      )
(map! :map (minibuffer-local-completion-map evil-ex-completion-map)
      :i "TAB" #'minibuffer-complete
      )

(after! yasnippet
  (map! :map yas-minor-mode-map :i "C-e" 'yas-expand)
  (map! :map yas-keymap :i "C-e" 'yas-next-field-or-maybe-expand)
  ; Can't seem to get this to stop using tab to expand when after
  ; a snippet, but tbh I think it's minor. Doesn't affect company.
  (dolist (keymap (list yas-minor-mode-map yas-keymap))
    (dolist (tab (list "TAB" "<tab>"))
      (map! :map keymap :i tab nil))))

;; c-a and c-x need fixing for increment/decrement.

;; bind page up and down to the evil-motion-state-map in order to get them only
;; functioning in read-only modes.

;; Latex mode mappings
(map!
 :after latex
 :map LaTeX-mode-map
 :localleader
 :desc "View" "v" #'TeX-view
 :desc "Compile all" "l" #'TeX-command-run-all)


;; SPC r is unused in default doom emacs.
;; (map! :leader :desc "Inline code evaluate" :nv "r" #'eval:region)
;; (map! :leader :desc "Inline code evaluate" :nv "rr" #'eval:region)
;; (map! :leader :desc "Send to REPL" :v "r" #'eval/send-region-to-repl)

(evil-ex-define-cmd "os" #'doom/load-session) ; Same as sl
(evil-ex-define-cmd "ss" #'doom/save-session)
(evil-ex-define-cmd "cs" #'doom/close-session)

;; gp to open ivy minibuffer for paste history.
(map! :n "gp" #'consult-yank-pop)


(evil-define-operator my/evil-replace-with-kill-ring (beg end)
  "Replace with killring action."
  :move-point nil (interactive "<r>")
  (save-excursion (delete-region beg end)
                  (goto-char beg)
                  (call-interactively 'evil-paste-before 1)))
(map! :n "d" (general-key-dispatch #'evil-delete
               "d" #'evil-delete-whole-line
               "r" #'my/evil-replace-with-kill-ring))
;; v inherits n-mode mappings for d, so explictly map to delete in visual mode.
(map! :v "d" #'evil-delete)
;; This may be more flexible instead of evil-delete-whole-line.
;; ('evil-change "d")

(map! :vo "a%" #'+evil:whole-buffer-txtobj
      :vo "i%" #'+evil:whole-buffer-txtobj)

;;; Vterm
(map! :n "C-s" #'+vterm/toggle)
(map! :after vterm
      :map vterm-mode-map
      :ni "C-s" #'+vterm/toggle
      ;; Undo the key chord (!) so that kv is passed to shell to enter the shell
      ;; normal mode.
      :i (general-chord "kv") "kv"
      :i (general-chord "vk") "vk"
      ;; shift+space catches most vterm C-c bindings.
      :ni "C-c" #'vterm-send-C-c
      )

;; (add-hook! 'dired-mode-hook (setq doom-localleader-key "\\"))
(map! :map dired-mode-map
      "RET" #'dired-find-file
      "e" #'dired-find-file
      "backspace" #'dired-up-directory)

(map! :n "y" (general-key-dispatch #'evil-yank
                "y" #'evil-collection-magit-yank-whole-line
                "o" doom-leader-toggle-map))
(map! :v "y" #'evil-yank)

;;;###autoload
(defun my/toggle-search-highlight ()
  (interactive)
  (setq evil-ex-substitute-highlight-all (not evil-ex-substitute-highlight-all))
  (setq evil-ex-search-persistent-highlight (not evil-ex-search-persistent-highlight))
  ;; (evil-ex-nohighlight)
  (redraw-display))
(map! :leader
      (:prefix "t"
       ;; :map doom-leader-toggle-map
       :desc "Search result highlighting" "h" #'evil-ex-nohighlight
       :desc "whitespace-mode" "l" #'whitespace-mode
       :desc "Center text in window" "=" #'centered-window-mode-toggle
       :desc "word wrap" "w" #'toggle-truncate-lines
       ;; evil respects visual-line-mode, but not truncate-lines. In other
       ;; words, enabling visual-line-mode (aka word-wrap-mode) will enable
       ;; visual line motions (j=gj)
       :desc "wrapped line motion" "g" #'visual-line-mode
       :desc "line numbers" "n" #'doom/toggle-line-numbers
       :desc "Subword mode" "m" #'subword-mode
       :desc "Superword mode" "M" #'superword-mode
       :desc "Super-save autosave" "a" #'super-save-mode))

;; Override evil-unimpaired, return to what doom uses.
(add-hook! 'evil-collection-setup-hook
  (map! :map evil-collection-unimpaired-mode-map :nv "]e" #'next-error)
  (map! :map evil-collection-unimpaired-mode-map :nv "[e" #'previous-error)
  )

(after! helm
  ;; I want backspace to go up a level, like ivy
  (add-hook! 'helm-find-files-after-init-hook
    (map! :map helm-find-files-map
          "<DEL>" #'helm-find-files-up-one-level)))

(map! :map evil-inner-text-objects-map "," #'evil-inner-arg)
(map! :map evil-outer-text-objects-map "," #'evil-outer-arg)

(map! :i "M-/" #'hippie-expand)

;; (map! :nv "S" #'my/evil-motion-subword-once)

(map! :nv "gx" #'browse-url)


(after! evil-little-word
  (map! :mv "_" nil)
  ;; Subword/camelcasemotion equivalents
  (map! :mv "_w" 'evil-forward-little-word-begin)
  (map! :mv "_b" 'evil-backward-little-word-begin)
  (map! :mv "_e" 'evil-forward-little-word-end))
;; (map! :mv "_" 'evil-backward-little-word-end)
;; (map! evil-outer-text-objects-map (kbd "lw") 'evil-a-little-word)
;; (map! evil-inner-text-objects-map (kbd "lw") 'evil-inner-little-word)
;; (map! :map evil-motion-state-map "_" #'my/evil-motion-subword-once)
(require 'evil-little-word)


(dolist (tab (list "TAB" "<tab>"))
  (map! :after markdown
        :map markdown-mode-map
        :i
        tab nil))

(map! :localleader
      :map markdown-mode-map
      :prefix ("i" . "Insert")
      :desc "Blockquote"    "q" 'markdown-insert-blockquote
      :desc "Bold"          "b" 'markdown-insert-bold
      :desc "Code"          "c" 'markdown-insert-code
      :desc "Emphasis"      "e" 'markdown-insert-italic
      :desc "Footnote"      "f" 'markdown-insert-footnote
      :desc "Code Block"    "s" 'markdown-insert-gfm-code-block
      :desc "Image"         "i" 'markdown-insert-image
      :desc "Link"          "l" 'markdown-insert-link
      :desc "List Item"     "n" 'markdown-insert-list-item
      :desc "Pre"           "p" 'markdown-insert-pre
      (:prefix ("h" . "Headings")
        :desc "One"   "1" 'markdown-insert-atx-1
        :desc "Two"   "2" 'markdown-insert-atx-2
        :desc "Three" "3" 'markdown-insert-atx-3
        :desc "Four"  "4" 'markdown-insert-atx-4
        :desc "Five"  "5" 'markdown-insert-atx-5
        :desc "Six"   "6" 'markdown-insert-atx-6))

;; (after! org (map! :localleader
;;       :map org-mode-map
;;       :desc "Eval Block" "e" 'ober-eval-block-in-repl
;;       (:prefix "o"
;;         :desc "Tags" "t" 'org-set-tags
;;         :desc "Roam Bibtex" "b" 'orb-note-actions
;;         (:prefix ("p" . "Properties")
;;           :desc "Set" "s" 'org-set-property
;;           :desc "Delete" "d" 'org-delete-property
;;           :desc "Actions" "a" 'org-property-action
;;           )
;;         )
;;       (:prefix ("i" . "Insert")
;;         :desc "Link/Image" "l" 'org-insert-link
;;         :desc "Item" "o" 'org-toggle-item
;;         :desc "Citation" "c" 'org-ref-helm-insert-cite-link
;;         :desc "Footnote" "f" 'org-footnote-action
;;         :desc "Table" "t" 'org-table-create-or-convert-from-region
;;         :desc "Screenshot" "s" 'org-download-screenshot
;;         (:prefix ("b" . "Math")
;;          :desc "Bold" "f" 'org-make-bold-math
;;          :desc "Blackboard" "b" 'org-make-blackboard-math
;;          :desc "Vert" "v" 'org-make-vert-math
;;          )
;;         (:prefix ("h" . "Headings")
;;           :desc "Normal" "h" 'org-insert-heading
;;           :desc "Todo" "t" 'org-insert-todo-heading
;;           (:prefix ("s" . "Subheadings")
;;             :desc "Normal" "s" 'org-insert-subheading
;;             :desc "Todo" "t" 'org-insert-todo-subheading
;;             )
;;           )
;;         (:prefix ("e" . "Exports")
;;           :desc "Dispatch" "d" 'org-export-dispatch
;;           )
;;         )
;;       )
;;   )

 ;; (defhydra my/hydra-scroll-mode (global-map "M-r")
 ;;    ("d" #'evil-scroll-down)
 ;;    ("f" )
 ;;    ("b" )
 ;;    ("p" )
 ;;    )

;; Imitate abolish.vim's coersion
;; Cycle between snake case, camel case, etc.
;; (map! "crs" 'string-inflection-cycle)
;; (map! :n "crm" 's-upper-camelcase)        ;; Force to MixedCase
;; (map! :n "crc" 's-lower-camelcase)  ;; Force to camelCase
;; ;; (map! :n "cru" 'string-inflection-lower-camelcase)  ;; Force to SNAKE_UPPERCASE
;; (map! :n "crs" 'string-inflection-underscore) ;; Force to snake_case
;; (map! :n "cr<space>" 's-split-words) ;; Force to space case
;; (map! :n "cr-" 'string-inflection-underscore) ;; Force to dash-case
;; (string-inflection-underscore-function "EmacsLisp")           ; => "emacs_lisp"
;; (string-inflection-pascal-case-function "emacs_lisp")         ; => "EmacsLisp"
;; (string-inflection-camelcase-function "emacs_lisp")           ; => "emacsLisp"
;; (string-inflection-upcase-function "emacs_lisp")              ; => "EMACS_LISP"
;; (string-inflection-kebab-case-function "emacs_lisp")          ; => "emacs-lisp"
;; (string-inflection-capital-underscore-function "emacs_lisp")  ; => "Emacs_Lisp"
