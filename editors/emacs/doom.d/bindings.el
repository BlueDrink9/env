;;; mappings.el -*- lexical-binding: t; -*-

(setq doom-localleader-key "<return>")

;; Requires KeyChord library
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.5)
(map! :desc "Enter normal mode" :i (general-chord "kv") 'evil-normal-state)
(map! :desc "Enter normal mode" :i (general-chord "vk") 'evil-normal-state)

;; Swap ;, :
(map! :desc "ex" :nv ";" #'evil-ex)
(map! :desc ";" :nv ":" #'evil-repeat-find-char)
;; Snipe gives colour hints.
(map! :desc ";" :nv ":" #'evil-snipe-repeat-forwards)

(map! :desc "go-window-right" :nv  "C-l" #'evil-window-right)
(map! :desc "go-window-left"  :nv  "C-h" #'evil-window-left)
(map! :desc "go-window-up"    :nv  "C-k" #'evil-window-up)
(map! :desc "go-window-down"  :nv  "C-j" #'evil-window-down)

;; (map! :after outline
;;       :map outline-mode-map
;;       <C-j> nil
;;       <C-k> nil)

(map! :desc "split window right" :nv  "C-w l" #'evil-window-vsplit)
(map! :desc "split window down" :nv  "C-w j" #'evil-window-split)

(map! :map evil-window-map
      "t"       #'+workspace/new
      ;; Swapping windows
      "S-<left>"       #'+evil/window-move-left
      "S-<down>"       #'+evil/window-move-down
      "S-<up>"         #'+evil/window-move-up
      "S-<right>"      #'+evil/window-move-right)

(map! :desc "buf-next" :n  "<right>" #'centaur-tabs-forward)
(map! :desc "buf-next" :n  "<left>" #'centaur-tabs-backward)
(map! :desc "buf-next" :n  "<C-tab>" #'centaur-tabs-forward)
(map! :desc "buf-next" :n  "<C-iso-lefttab>" #'centaur-tabs-backward)
;; Unsure if I want tabs or workspaces.
;; (map! :desc "buf-next" :nv  "<up>" #'tabnext)
;; (map! :desc "buf-next" :nv  "<down>" #'evil-ex "tabprev")


(map! :leader :desc "Comment" :nv "c" #'evilnc-comment-operator)
(map! :leader :desc "Comment" :nv "C-/" #'evilnc-comment-operator)

(map! :desc "Describe key" :nv "C-?" #'describe-key-briefly)

(defun my/evil-paste-from-system ()
  "Paste from system register ('+') "
  (interactive)
  (evil-paste-from-register ?+))

(defun my/evil-use-system-register-next ()
  "Sets the register for the next action to the system register"
  (interactive)
  ;; (evil-use-register ?+))
  (setq evil-this-register ?+))
;; (map! :desc "system register" :nv (general-chord "\"\"") #'my/evil-use-system-register-next)

;; TODO: Make this not override the unnamed register
(map! :desc "Use system register next" :nv (general-chord "\"\"") "\"+")
(map! :desc "paste from clipboard" :i "C-S-v" #'my/evil-paste-from-system)
(map! :desc "Paste from clipboard" :i "C-v" #'my/evil-paste-from-system)

;; maps for everywhere (all modes)
;; (with-eval-after-load 'evil-maps
  ;; (define-key evil-motion-state-map (kbd "C-?") 'describe-key-briefly)
  ;; )
;; maps for specific modes
;; (evil-define-key 'normal org-mode-map "\<" 'org-metaleft )

;; Don't add to register on x or X
(map! :desc "\"_x" :nv  "x" #'delete-forward-char)
(map! :desc "\"_X" :nv  "X" #'delete-backward-char)
(map! :desc "black hole register delete" :v  "<delete>" #'evil-delete-char)

;; (map! :desc "Toggle fold" :n  "<backspace>" #'+fold/toggle)
;; Use the keys (za) rather than the function so that it works with more
;; evilified minor modes (eg magit).
(map! :desc "Toggle fold" :n  "<backspace>" "za")
(map! :desc "Create fold from selection" :v  "<backspace>" #'evil-vimish-fold/create)

(map! :desc "Go to mark" :n  "'" #'evil-goto-mark)
(map! :desc "Go to mark line" :n  "`" #'evil-goto-mark-line)

;; (map! :desc "Dot operator without moving" :n  "." #'evil-repeat | evil-goto-mark(".")) ; want to replicate .'.

;; Insertmode deletes create an undopoint first
;; inoremap <c-u> <c-g>u<c-u>

(map! :desc "Faster redo" :n  "U" #'evil-redo)

(map! :desc "Go to alternate file" :n  "SPC a" #'evil-switch-to-windows-last-buffer)
(map! :desc "Run most recent macro" :n  "Q" #'evil-execute-macro)

;;  by default keeps C-x and c-a for emacs things.
(map! :desc "Increment number" :n  "C-a" #'evil-numbers/inc-at-pt)
(map! :desc "Decrement number" :n  "C-x" #'evil-numbers/dec-at-pt)
(map! :leader :desc "ctrl x replacement" :nv "C-x" #'evilnc-comment-operator)

;; (map! :desc "CD to current file's dir" :n  "SPC c d" #')

; n and N always go the same direction regardless of whether / or ? was used.

;; ;; Autoexpand brackets when creating functions etc.
;; (defun autobracket (bracCharOpen bracCharClose)
;;     (insert bracCharOpen)
;;     (evil-insert-newline-below)
;;     (insert bracCharClose)
;;     (evil-previous-line)
;;     (evil-insert-newline-below)
;;   )
;; ;; This mostly works but will not ever trigger properly because key-chord.el cannot bind return key!
;; (map! :desc "Autoexpand bracket" :i
;;       (general-chord (concat "(" (kbd "<RET>")))
;;       (lambda () (interactive) (autobracket "(" ")")))

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
(map! :desc "Insert a single char before cursor" :n "s" #'my/evil-insert-char)

(map! :desc "Easymotion prefix" :n "S" nil)
(after! evil-easymotion
  (evilem-default-keybindings "S")
  (map! :n "S l" #'evilem-motion-forward-word-begin)
  (map! :n "S h" #'evilem-motion-backward-word-begin)
  )

;; Git/magit shortcuts
(map! :leader
      (:prefix ("g")
        :desc "pusH to origin" "h" #'magit-push-current-to-pushremote
        :desc "pull from origin" "p" #'magit-pull-from-pushremote))
(evil-ex-define-cmd "gc" 'magic-commit-create)
(evil-ex-define-cmd "gw" 'magit-stage-file)

;; (map! :leader
;;       (:prefix-map ("a" . "applications")
;;        (:prefix ("j" . "journal")
;;         :desc "New journal entry" "j" #'org-journal-new-entry
;;         :desc "Search journal entry" "s" #'org-journal-search)))

;; Apparently have to add mapping to every file mode?
(after! smart-tab
  (add-hook 'find-file-hook (function (lambda ()
                                        (local-set-key (kbd "<tab>") 'smart-tab))))
  )

(map! :desc "Minibuffer only one escape to exit"
      :map (minibuffer-local-map evil-ex-completion-map evil-ex-search-keymap)
      :in "<escape>" #'abort-recursive-edit)

;; c-a and c-x need fixing for increment/decrement.

;; bind page up and down to the evil-motion-state-map in order to get them only
;; functioning in read-only modes.

;; Latex mode mappings
(map!
 :map LaTeX-mode-map
 :localleader
 :desc "View" "v" #'TeX-view
 :desc "Compile all" "l" #'TeX-command-run-all)


(map! :leader :desc "Inline code evaluate" :nv "r" #'eval:region)
;; (map! :leader :desc "Inline code evaluate" :nv "rr" #'eval:region)
;; (map! :leader :desc "Send to REPL" :v "r" #'eval/send-region-to-repl)

(evil-ex-define-cmd "os" #'doom/load-session) ; Same as sl
(evil-ex-define-cmd "ss" #'doom/save-session)
(evil-ex-define-cmd "cs" #'doom/close-session)

;; gp to open ivy minibuffer for paste history.
(map! :n "gp" #'counsel-yank-pop)


(evil-define-operator my/evil-replace-with-kill-ring (beg end)
  "Replace with killring action."
  :move-point nil (interactive "<r>")
  (save-excursion (delete-region beg end)
                  (goto-char beg)
                  (call-interactively 'evil-paste-before 1)))
(map! :n "d" (general-key-dispatch 'evil-delete
                    "r" 'my/evil-replace-with-kill-ring
                    "d" 'evil-delete-whole-line))
;; v inherits n-mode mappings for d, so explictly map to delete in visual mode.
(map! :v "d" #'evil-delete)
;; This may be more flexible instead of evil-delete-whole-line.
;; ('evil-change "d")

(map! :o "a%" 'mark-whole-buffer)
(map! :o "i%" 'mark-whole-buffer)
