;;; mappings.el -*- lexical-binding: t; -*-

(setq doom-localleader-key "<return>")

;; Requires KeyChord library
(setq key-chord-two-keys-delay 0.5)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "kv" 'evil-normal-state)
(key-chord-define evil-insert-state-map "vk" 'evil-normal-state)
; (key-chord-define evil-ex-state-map "kv" 'evil-command-window-ex)
; (key-chord-define evil-ex-state-map "vk" 'evil-command-window-ex)

; (map! :desc "ex normal" :x ";" #'evil-ex)

;; Swap ;, :
(map! :desc "ex" :nv ";" #'evil-ex)
(map! :desc ";" :nv ":" #'evil-repeat-find-char)

(map! :desc "go-window-right" :nv  "C-l" #'evil-window-right)
(map! :desc "go-window-left"  :nv  "C-h" #'evil-window-left)
(map! :desc "go-window-up"    :nv  "C-k" #'evil-window-up)
(map! :desc "go-window-down"  :nv  "C-j" #'evil-window-down)

(map! :after outline
      :map outline-mode-map
      <C-j> nil
      <C-k> nil)

(map! :desc "split window right" :nv  "C-w l" #'evil-window-vsplit)
(map! :desc "split window down" :nv  "C-w j" #'evil-window-split)

(map! :desc "buf-next" :nv  "<right>" #'next-buffer)
(map! :desc "buf-next" :nv  "<left>" #'previous-buffer)
(map! :desc "buf-next" :nv  "<up>" #'workspace/switch-right)
(map! :desc "buf-next" :nv  "<down>" #'workspace/switch-left)


(map! :leader :desc "Comment" :nv "c" #'evilnc-comment-operator)
(map! :leader :desc "Comment" :nv "C-/" #'evilnc-comment-operator)

(map! :desc "Describe key" :nv "C-?" #'describe-key-briefly)

;; (defun myevil-use-system-register() "" () (evil-use-register "+"))

;; (map! :desc "system register" :nv "\"\"" #'(setq evil-this-register "+"))
;; (map! :desc "system register" :nv "\"\"\"" #'myevil-use-system-register)
;; (map! :nv "C-z" (setq evil-this-register ?+))
;; (key-chord-define evil-motion-state-map "''" (setq evil-this-register "+"))

;; (map! :desc "system register" :nv  '""' '"+')
;; maps for everywhere (all modes)
;; (with-eval-after-load 'evil-maps
  ;; (define-key evil-motion-state-map (kbd "C-?") 'describe-key-briefly)
  ;; )
;; maps for specific modes
;; (evil-define-key 'normal org-mode-map "\<" 'org-metaleft )

(map! :desc "_x" :nv  "x" #'evil-delete-char)
(map! :desc "_X" :nv  "X" #'evil-backward-char)
(map! :desc "black hole register delete" :v  "<backspace>" #'evil-delete-char)
(map! :desc "black hole register delete" :v  "<delete>" #'evil-delete-char)

(map! :desc "Toggle fold" :n  "<backspace>" #'+fold/toggle)
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
; May need to be a chord.
;; (map! :desc "Autoexpand bracket" :i  "( <cr>" "(<cr>)<Esc>O")
;; inoremap (<CR> (<CR>)<Esc>O
;; inoremap {<CR> {<CR>}<Esc>O
;; inoremap {; {<CR>};<Esc>O
;; inoremap {, {<CR>},<Esc>O
;; inoremap [<CR> [<CR>]<Esc>O
;; inoremap [; [<CR>];<Esc>O
;; inoremap [, [<CR>],<Esc>O

; Baisc mappings done as of now.

(map! :leader :desc "easymotion down" "j" #'evil-motion-next-line)
(map! :leader :desc "easymotion up" "k" #'evil-motion-previous-line)

;; (map! :leader
;;       (:prefix-map ("a" . "applications")
;;        (:prefix ("j" . "journal")
;;         :desc "New journal entry" "j" #'org-journal-new-entry
;;         :desc "Search journal entry" "s" #'org-journal-search)))