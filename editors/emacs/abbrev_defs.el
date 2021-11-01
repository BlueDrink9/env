;;; abbrev_defs.el  -*- lexical-binding: t; -*-

;;; This lets you use special characters in abbrevs.
;;; Stolen from https://emacs.stackexchange.com/questions/36890/alternative-to-abbrev-mode-that-supports-special-characters-in-abbreviations
(defcustom abbrev-additional-chars
  '((t ?-))
  "Alist that maps major mode symbols to lists of characters that may appear in abbreviations.
The chars of the special major mode symbol `t' are active in all modes."
  :group 'abbrev
  :type '(repeat :tag "List of modes"
                 (cons :tag "Map major mode symbols to lists of additional chars in abbrevs"
                       (symbol :tag "Mode symbol (`t' stands for all modes)")
                       (repeat :tag "List of additional word-consistent characters" character))))

(defvar-local T-abbrev-syntax-table nil
  "List of additional characters in abbreviations.")

(defun T-abbrev-mode-hook-fun ()
  "Populate T-abbrev-syntax-table with the local syntax table modfied by
the characters in `abbrev-additional-chars'."
  (when abbrev-mode
    (let ((char-list (append (cdr (assoc major-mode abbrev-additional-chars))
                             (cdr (assoc 't abbrev-additional-chars)))))
      (setq T-abbrev-syntax-table (make-syntax-table (syntax-table)))
      (mapcar (lambda (char)
                (modify-syntax-entry char "w" T-abbrev-syntax-table))
              char-list))))

;; Wrapping functions of the `abbrev` package with the local syntax table.
;; I'm not sure I captured all fun's that need to run with the local syntax-table.
;; Adding further functions is easy.
;; Just add them to the list at the end of the next form.
(mapcar
 (lambda (fun)
   (let ((newfun (intern (concat "T-ad-" (symbol-name fun)))))
     (eval
      `(progn
         (defun ,newfun (oldfun &rest args)
           ,(concat "This function evaluates `" (symbol-name fun) "' with `T-abbrev-syntax-table' as active syntax table.
It is used for the advicing `" (symbol-name fun) "'.")
           (with-syntax-table T-abbrev-syntax-table
             (apply oldfun args)
             ))
         (advice-add (quote ,fun) :around (quote ,newfun))))))
 '(define-mode-abbrev abbrev--before-point))

(add-hook 'abbrev-mode-hook #'T-abbrev-mode-hook-fun)
(setq abbrev-additional-chars '((t ?[ ?])))

(defun my/insert-current-iso-date ()
  "insert iso date"
  (insert (format-time-string "%Y-%m-%d")))
;; Note: for now, the above does not work when finishing on a non-standard symbol.
(define-abbrev global-abbrev-table "[d" "" #'my/insert-current-iso-date)
