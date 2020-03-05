;;; big-mode-el -- Major mode for editing BigraphER model files

(defvar big-mode-hook nil)
(defvar big-mode-map
  (let ((big-mode-map (make-keymap)))
    (define-key big-mode-map "\C-j" 'newline-and-indent)
    big-mode-map)
  "Keymap for BigraphER major mode")

(add-to-list 'auto-mode-alist '("\\.big\\'" . big-mode))

(defconst big-types
  (eval-when-compile
    (regexp-opt
     '("big"
       "ctrl"
       "float"
       "int"
       "string"
       "react")))
  "BigraphER mode types.")

(defconst big-keywords
  (eval-when-compile
    (regexp-opt
     '("fun"
       "brs"
       "end"
       "sbrs"
       "pbrs"
       "nbrs"
       "begin"
       "init"
       "atomic"
       "preds"
       "rules"
       "action")))
  "BigraphER mode keywords.")

(defconst big-exp
  (eval-when-compile
    (regexp-opt
     '("->"
       "-->"
       "-["
       "]->"
       "@"
       "by"
       "in"
       "share"
       "par"
       "ppar"
       )))
  "BigraphER mode expressions.")

(defconst big-const
  (eval-when-compile
    (regexp-opt
     '("true"
       "false"
       "inf")))
  "BigraphER mode constants.")

(defconst big-font-lock-keywords-1
  (append
   (list
    ;; Fontify constants
    (cons
     (concat "\\<\\(" big-const "\\)\\>")
     '(1 font-lock-constant-face))
    ;; Fontify keywords
    (cons
     (concat "\\<\\(" big-keywords "\\)\\>")
     '(1 font-lock-builtin-face))
    ;; Fontify types
    (cons
     (concat "\\<\\(" big-types "\\)\\>")
     '(1 font-lock-type-face))
    ;; Fontify expressions
    (cons
     (concat "\\<\\(" big-exp "\\)\\>")
     '(1 font-lock-keyword-face))))
  "Minimal level highlighting for BigraphER mode.")

(defconst big-font-lock-keywords-2
  (append
   big-font-lock-keywords-1
   (list
    ;; Fontify var names
    (list
     (concat "\\(" big-types "\\)\\s-\\(\[^(\s-\]+\\)")
     '(2 font-lock-function-name-face))
    ;; Fontify formals
    ;;(list
    ;; "fun\[^(\]+(\\(\\(.*,\\)*\[^,)\]+\\)\[^)\]*)"
    ;; "fun\[^(\]+(\\(\[^,)\]+\\)" ;; first formal
    ;; "fun\[^(\]+(\[^,\]+,\\(\[^,)\]+\\)" ;; second
    ;; '(1 font-lock-variable-name-face)
    ))
  "Medium level highlighting for BigraphER mode.")

(defvar big-font-lock-keywords big-font-lock-keywords-2
  "Default highlighting expressions for BigraphER mode.")

(defun big-indent-line ()
  "Indent current line as big code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)   ; First line is always non-indented
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*\\(end\\|)\\|;\\)") ; If the line we are looking at is the end of a block, then decrease the indentation
	  (progn
	    (save-excursion
	      (forward-line -1)
	      (setq cur-indent (- (current-indentation) 2)))
	    (if (< cur-indent 0) ; We can't indent past the left margin
		(setq cur-indent 0)))
	(save-excursion
	  (while not-indented ; Iterate backwards until we find an indentation hint
	    (forward-line -1)
	    (if (looking-at "^[ \t]*\\(end\\|)\\)") ; This hint indicates that we need to indent at the level of the end_ token
		(progn
		  (setq cur-indent (current-indentation))
		  (setq not-indented nil))
	      (if (looking-at "^[ \t]*\\(=\\|(\\|begin\\|action\\)") ; This hint indicates that we need to indent an extra level
		  (progn
		    (setq cur-indent (+ (current-indentation) 2)) ; Do the actual indenting
		    (setq not-indented nil))
		(if (bobp)
		    (setq not-indented nil)))))))
      (if cur-indent
	  (indent-line-to cur-indent)
	(indent-line-to 0))))) ; If we didn't see an indentation hint, then allow no indentation

(defvar big-mode-syntax-table
  (let ((big-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" big-mode-syntax-table)
    (modify-syntax-entry ?# "< b" big-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" big-mode-syntax-table)
    big-mode-syntax-table)
  "Syntax table for big-mode")

(defun big-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map big-mode-map)
  (set-syntax-table big-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(big-font-lock-keywords))
  ;; Register our indentation function
  (set (make-local-variable 'indent-line-function) 'big-indent-line)  
  (setq major-mode 'big-mode)
  (setq mode-name "BIG")
  (run-hooks 'big-mode-hook))

(provide 'big-mode)

