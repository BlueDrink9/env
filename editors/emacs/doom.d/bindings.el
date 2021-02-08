;;; mappings.el -*- lexical-binding: t; -*-

;; Requires KeyChord library
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "kv" 'evil-normal-state)
(key-chord-define evil-insert-state-map "vk" 'evil-normal-state)
(key-chord-mode 1)

;; Swap ;, :
(map! :desc "ex" :nv ";" #'evil-ex)
(map! :desc ";" :nv ":" #'evil-repeat-find-char)

(map! :desc "go-window-right" :nv  "C-l" #'evil-window-right)
(map! :desc "go-window-left" :nv  "C-h" #'evil-window-left)
(map! :desc "go-window-up" :nv  "C-k" #'evil-window-up)
(map! :desc "go-window-down" :nv  "C-j" #'evil-window-down)

(map! :desc "split window right" :nv  "C-w l" #'evil-window-vsplit)
(map! :desc "split window down" :nv  "C-w j" #'evil-window-split)

(map! :desc "buf-next" :nv  "<right>" #'next-buffer)
(map! :desc "buf-next" :nv  "<left>" #'previous-buffer)
(map! :desc "buf-next" :nv  "<up>" #'workspace/switch-right)
(map! :desc "buf-next" :nv  "<down>" #'workspace/switch-left)


(map! :leader :desc "Comment" :nv "c" #'evilnc-comment-operator)

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


;; (map! :leader
;;       (:prefix-map ("a" . "applications")
;;        (:prefix ("j" . "journal")
;;         :desc "New journal entry" "j" #'org-journal-new-entry
;;         :desc "Search journal entry" "s" #'org-journal-search)))
