;;; mappings.el -*- lexical-binding: t; -*-

(setq doom-localleader-key "<return>")

;; Requires KeyChord library
(setq key-chord-two-keys-delay 0.5)
(key-chord-mode 1)
;; (key-chord-define evil-insert-state-map "kv" 'evil-normal-state)
;; (general-define-key :keymaps 'evil-insert-state-map
;;                     (general-chord "kv") 'evil-normal-state
;;                     (general-chord "vk") 'evil-normal-state
;;                     )
(map! :desc "Enter normal mode" :i (general-chord "kv") 'evil-normal-state)
(map! :desc "Enter normal mode" :i (general-chord "vk") 'evil-normal-state)
;; (key-chord-define evil-insert-state-map "vk" 'evil-normal-state)
; (key-chord-define evil-ex-state-map "kv" 'evil-command-window-ex)
; (key-chord-define evil-ex-state-map "vk" 'evil-command-window-ex)

;; ;; Alternative solution using general.el
;; (general-imap "v"
;;               (general-key-dispatch 'self-insert-command
;;                 :timeout 0.5
;;                 "k" 'evil-normal-state))

; (map! :desc "ex normal" :x ";" #'evil-ex)

;; Swap ;, :
(map! :desc "ex" :nv ";" #'evil-ex)
(map! :desc ";" :nv ":" #'evil-repeat-find-char)
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

(map! :desc "buf-next" :nv  "<right>" #'next-buffer)
(map! :desc "buf-next" :nv  "<left>" #'previous-buffer)
;; Unsure if I want tabs or workspaces.
;; (map! :desc "buf-next" :nv  "<up>" #'tabnext)
;; (map! :desc "buf-next" :nv  "<down>" #'evil-ex "tabprev")


(map! :leader :desc "Comment" :nv "c" #'evilnc-comment-operator)
(map! :leader :desc "Comment" :nv "C-/" #'evilnc-comment-operator)

(map! :desc "Describe key" :nv "C-?" #'describe-key-briefly)

;; (defun myevil-use-system-register() "" () (evil-use-register "+"))
;; (general-nmap "\"" (general-key-dispatch
;;                        "\"" 'evil-use-register "+"))

(defun myevil-paste-from-system ()
  "Paste from system register ('+') "
  (interactive)
  (evil-paste-from-register ?+))

(defun myevil-use-system-register-next ()
  "Sets the register for the next action to the system register"
  (interactive)
  ;; (evil-use-register ?+))
  (setq evil-this-register ?+))
;; (map! :desc "system register" :nv (general-chord "\"\"") #'myevil-use-system-register-next)

;; TODO: Make this not override the unnamed register
(map! :desc "Use system register next" :nv (general-chord "\"\"") "\"+")
(map! :desc "Paste from clipboard" :i "C-S-v" #'myevil-paste-from-system)
(map! :desc "Paste from clipboard" :i "C-v" #'myevil-paste-from-system)

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

;; (map! :desc "CD to current file's dir" :n  "SPC c d" #')

; n and N always go the same direction regardless of whether / or ? was used.

;; Autoexpand brackets when creating functions etc.
(defmacro autobracket (bracCharOpen BracCharClose)
  (
    (insert bracCharOpen)
    (evil-insert-newline-below)
    (insert bracCharClose)
    (evil-previous-line)
    (evil-insert-newline-below)
    )
  )
;; (general-imap "(" (general-key-dispatch "RET" 'autobracket "(" ")"))

;; ;; May need to be a chord.
;; Currently working, but isn't typing regular ( when no RET after it.
;; (map! :desc "Autoexpand bracket" :i
;;       "( RET" (lambda () (interactive) (autobracket "(" ")")))

               ;; lambda () (interactive)
               ;;  (insert "(")
               ;; (evil-insert-newline-below)
               ;; (insert ")")
               ;; (evil-previous-line)
               ;; (evil-insert-newline-below)
               ;; ))

;; (map! :desc "Autoexpand bracket" :i  "( <cr>" "(<cr>)<Esc>O")
;; inoremap (<CR> (<CR>)<Esc>O
;; inoremap {<CR> {<CR>}<Esc>O
;; inoremap {; {<CR>};<Esc>O
;; inoremap {, {<CR>},<Esc>O
;; inoremap [<CR> [<CR>]<Esc>O
;; inoremap [; [<CR>];<Esc>O
;; inoremap [, [<CR>],<Esc>O

; Basic mappings done as of now.

;; (map! :leader :desc "easymotion down" "j" #'evil-motion-next-line)
;; (map! :leader :desc "easymotion up" "k" #'evil-motion-previous-line)

(map! :leader
      (:prefix ("g")
        :desc "pusH to origin" "h" #'magit-push-current-to-pushremote
        :desc "pull from origin" "p" #'magit-pull-from-pushremote))

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
 :desc "View" "v" #'TeX-view)


(map! :leader :desc "Inline code evaluate" :nv "r" #'eval:region)
;; (map! :leader :desc "Inline code evaluate" :nv "rr" #'eval:region)
;; (map! :leader :desc "Send to REPL" :v "r" #'eval/send-region-to-repl)

(evil-ex-define-cmd "os" 'doom/load-session) ; Same as sl
(evil-ex-define-cmd "ss" 'doom/save-session)
(evil-ex-define-cmd "cs" 'doom/close-session)
