;;; hs-indent-kb.el -- indentation module for Haskell Mode

;; Copyright 2009 Kristof Bastiaensen

;; Author: 2009 Kristof Bastiaensen <kristof.bastiaensen@vleeuwen.org>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'syntax nil t)			; Emacs 21 add-on

(defgroup hs-indent-kb nil
  "Haskell indentation."
  :group 'haskell
  :prefix "hs-indent-kb-")

(defcustom hs-indent-kb-cycle-warn t
  "Warn before moving to the leftmost indentation, if you tab at the rightmost one."
  :type 'boolean
  :group 'hs-indent-kb)

(defcustom hs-indent-kb-layout-offset 2
  "Extra indentation to add before expressions in a haskell layout list."
  :type 'integer
  :group 'hs-indent-kb)

(defcustom hs-indent-kb-starter-offset 1
  "Extra indentation after an opening keyword (e.g. let)."
  :type 'integer
  :group 'hs-indent-kb)

(defcustom hs-indent-kb-left-offset 2
  "Extra indentation after an indentation to the left (e.g. after do)."
  :type 'integer
  :group 'hs-indent-kb)

(defcustom  hs-indent-kb-ifte-offset 2
  "Extra indentation after the keywords `if' `then' or `else'."
  :type 'integer
  :group 'hs-indent-kb)

(defcustom hs-indent-kb-where-pre-offset 2
  "Extra indentation before the keyword `where'."
  :type 'integer
  :group 'hs-indent-kb)

(defcustom hs-indent-kb-where-post-offset 2
  "Extra indentation after the keyword `where'."
  :type 'integer
  :group 'hs-indent-kb)

;; Avoid a global bogus definition (which the original run-time
;; `defun' made), and support Emacs 21 without the syntax.el add-on.
(eval-when-compile
  (unless (fboundp 'syntax-ppss)
    (defsubst syntax-ppss (&rest pos)
      (parse-partial-sexp (point-min) (or pos (point))))))

(defconst hs-indent-kb-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [?\r] 'haskell-newline-and-indent)
    (define-key keymap [backspace] 'hs-indent-kb-delete-backward-char)
    (define-key keymap [?\C-d] 'hs-indent-kb-delete-char)
    keymap))

;;;###autoload
(define-minor-mode hs-indent-kb-mode
  "Haskell indentation mode that deals with the layout rule.
It rebinds RET, DEL and BACKSPACE, so that indentations can be
set and deleted as if they were real tabs.  It supports
autofill-mode."
  :lighter " Ind"
  :keymap hs-indent-kb-mode-map
  (kill-local-variable 'indent-line-function)
  (kill-local-variable 'normal-auto-fill-function)
  (when hs-indent-kb-mode
    (setq max-lisp-eval-depth (max max-lisp-eval-depth 600)) ;; set a higher limit for recursion
    (set (make-local-variable 'indent-line-function)
         'hs-indent-kb-indent-line)
    (set (make-local-variable 'normal-auto-fill-function)
         'hs-indent-kb-auto-fill-function)
    (set (make-local-variable 'haskell-indent-last-position)
         nil)))

(defun turn-on-hs-indent-kb ()
  "Turn on the hs-indent-kb minor mode."
  (interactive)
  (hs-indent-kb-mode t))

(put 'parse-error
     'error-conditions
     '(error parse-error))
(put 'parse-error 'error-message "Parse error")

(defun parse-error (&rest args)
  (signal 'parse-error (apply 'format args)))

(defmacro on-parse-error (except &rest body)
  `(condition-case parse-error-string
       (progn ,@body)
     (parse-error
      ,except
      (message "%s" (cdr parse-error-string)))))

(defun haskell-current-column ()
  "Compute current column according to haskell syntax rules,
  correctly ignoring composition."
  (save-excursion
    (let ((start (point))
          (cc 0))
      (beginning-of-line)
      (while (< (point) start)
        (if (= (char-after) ?\t)
            (setq cc (* 8 (+ 1 (/ cc 8))))
          (incf cc))
        (forward-char))
      cc)))

(defun kill-indented-line (&optional arg)
  "`kill-line' for indented text.
Preserves indentation and removes extra whitespace"
  (interactive "P")
  (let ((col (haskell-current-column))
	(old-point (point)))
    (cond ((or (and (numberp arg) (< arg 0))
	       (and (not (looking-at "[ \t]*$"))
		    (or (not (numberp arg)) (zerop arg))))
					;use default behavior when calling with a negative argument
					;or killing (once) from the middle of a line
	   (kill-line arg))
	  ((and (skip-chars-backward " \t") ;always true
		(bolp)
		(save-excursion
		  (forward-line arg)
		  (not (looking-at "[ \t]*$"))))
					; killing from an empty line:
					; preserve indentation of the next line
	   (kill-region (point)
			(save-excursion
			  (forward-line arg)
			  (point)))
	   (skip-chars-forward " \t")
	   (if (> (haskell-current-column) col)
	       (move-to-column col)))
	  (t				; killing from not empty line:
					; kill all indentation
	   (goto-char old-point)
	   (kill-region (point)
			(save-excursion
			  (forward-line arg)
			  (skip-chars-forward " \t")
			  (point)))))))

(defun hs-indent-kb-auto-fill-function ()
  (when (> (haskell-current-column) fill-column)
    (while (> (haskell-current-column) fill-column)
      (skip-syntax-backward "-")
      (skip-syntax-backward "^-"))
    (let ((auto-fill-function nil)
	  (indent (car (last (hs-indent-kb-find-indentations)))))
      (newline)
      (indent-to indent)
      (end-of-line))))

(defun hs-indent-kb-reindent (col)
  (beginning-of-line)
  (delete-region (point)
		 (progn (skip-syntax-forward "-")
			(point)))
  (indent-to col))

(defun haskell-newline-and-indent ()
  (interactive)
  (on-parse-error (newline)
     (let* ((cc (haskell-current-column))
            (ci (current-indentation))
            (indentations (hs-indent-kb-find-indentations)))
       (skip-syntax-forward "-")
       (if (prog1 (and (eolp)
                       (not (= (haskell-current-column) ci)))
             (newline))
           (hs-indent-kb-reindent
            (max (hs-indent-kb-butlast indentations)
                 (hs-indent-kb-matching-indentation
                  ci indentations)))
         (hs-indent-kb-reindent (hs-indent-kb-matching-indentation
                                        cc indentations))))))

(defun hs-indent-kb-one-indentation (col indentations)
  (let* ((last-pair (last indentations)))
    (cond ((null indentations)
	   col)
	  ((null (cdr indentations))
	   (car indentations))
	  ((<= col (car last-pair))
	   col)
	  (t (car last-pair)))))

(defun hs-indent-kb-butlast (indentations)
  (when (consp (cdr indentations))
    (while (cddr indentations)
      (setq indentations (cdr indentations))))
  (car indentations))

(defun hs-indent-kb-next-indentation (col indentations)
  "Find the lefmost indentation which is greater than COL."
  (catch 'return
    (while indentations
      (if (or (< col (car indentations))
	      (null (cdr indentations)))
	  (throw 'return (car indentations))
	(setq indentations (cdr indentations))))
    col))

(defun hs-indent-kb-previous-indentation (col indentations)
  "Find the rightmost indentation which is less than COL."
  (and indentations
       (> col (car indentations))
       (catch 'return
	 (while indentations
	   (if (or (null (cdr indentations))
		   (<= col (cadr indentations)))
	       (throw 'return (car indentations))
	     (setq indentations (cdr indentations))))
	 col)))

(defun hs-indent-kb-matching-indentation (col indentations)
  "Find the leftmost indentation which is greater than or equal to COL."
  (catch 'return
    (while indentations
      (if (or (<= col (car indentations))
	      (null (cdr indentations)))
	  (throw 'return (car indentations))
	(setq indentations (cdr indentations))))
    col))

(defun hs-indent-kb-indent-line ()
  (when (save-excursion
	  (beginning-of-line)
	  (not (nth 8 (syntax-ppss))))
    (let ((ci (current-indentation))
          (start-column (haskell-current-column)))
      (cond ((> (haskell-current-column) ci)
	     (save-excursion
	       (move-to-column ci)
	       (hs-indent-kb-reindent
		(hs-indent-kb-one-indentation
		 ci (hs-indent-kb-find-indentations)))))

	    ((= (haskell-current-column) ci)
	     (hs-indent-kb-reindent
	      (hs-indent-kb-next-indentation
	       ci (hs-indent-kb-find-indentations))))

	    (t (move-to-column ci)
	       (hs-indent-kb-reindent
		(hs-indent-kb-matching-indentation
		 ci (hs-indent-kb-find-indentations)))))
      (cond ((not (= (haskell-current-column) start-column))
             (setq haskell-indent-last-position nil))
            ((not hs-indent-kb-cycle-warn)
             (hs-indent-kb-reindent
              (hs-indent-kb-next-indentation
               -1
               (hs-indent-kb-find-indentations))))
            ((not (equal (point) haskell-indent-last-position))
             (message "Press TAB again to go to the leftmost indentation")
             (setq haskell-indent-last-position (point)))
            (t
             (hs-indent-kb-reindent
              (hs-indent-kb-next-indentation
               -1
               (hs-indent-kb-find-indentations))))))))

(defun hs-indent-kb-delete-backward-char (n)
  (interactive "p")
  (on-parse-error (backward-delete-char 1)
     (cond
      ((and delete-selection-mode
            mark-active
            (not (= (point) (mark))))
       (delete-region (mark) (point)))
      ((or (= (haskell-current-column) 0)
           (> (haskell-current-column) (current-indentation))
           (nth 8 (syntax-ppss)))
       (delete-backward-char n))
      (t (let* ((ci (current-indentation))
                (pi (hs-indent-kb-previous-indentation
                     ci (hs-indent-kb-find-indentations))))
           (save-excursion
             (cond (pi
                    (move-to-column pi)
                    (delete-region (point)
                                   (progn (move-to-column ci)
                                          (point))))
                   (t
                    (beginning-of-line)
                    (delete-region (max (point-min) (- (point) 1))
                                   (progn (move-to-column ci)
                                          (point)))))))))))

(defun hs-indent-kb-delete-char (n)
  (interactive "p")
  (on-parse-error (delete-char 1)
    (cond
     ((and delete-selection-mode
           mark-active
           (not (= (point) (mark))))
      (delete-region (mark) (point)))
     ((or (eolp)
          (>= (haskell-current-column) (current-indentation))
          (nth 8 (syntax-ppss)))
      (delete-char n))
     (t
      (let* ((ci (current-indentation))
             (pi (hs-indent-kb-previous-indentation
                  ci (hs-indent-kb-find-indentations))))
        (save-excursion
          (if (and pi (> pi (haskell-current-column)))
              (move-to-column pi))
          (delete-region (point)
                         (progn (move-to-column ci)
                                (point)))))))))

(defun hs-indent-kb-goto-least-indentation ()
  (beginning-of-line)
  (catch 'return
    (while (not (bobp))
      (forward-comment (- (buffer-size)))
      (beginning-of-line)
      (let ((ps (nth 8 (syntax-ppss))))
		(when ps ;; inside comment or string
		  (goto-char ps)))
      (when (= 0 (current-indentation))
		(throw 'return nil))))
  (beginning-of-line)
  (when (bobp)
    (forward-comment (buffer-size))))

;; Dynamically scoped variables.
(defvar following-token)
(defvar current-token)
(defvar left-indent)
(defvar starter-indent)
(defvar current-indent)
(defvar layout-indent)
(defvar parse-line-number)
(defvar possible-indentations)
(defvar indentation-point)

(defun hs-indent-kb-parse-to-indentations ()
  (save-excursion
    (skip-syntax-forward "-")
    (let ((indentation-point (point))
	  (layout-indent 0)
	  (parse-line-number 0)
	  (current-indent hs-indent-kb-layout-offset)
	  (starter-indent hs-indent-kb-layout-offset)
	  (left-indent hs-indent-kb-layout-offset)
	  (case-fold-search nil)
	  current-token
	  following-token
	  possible-indentations)
      (hs-indent-kb-goto-least-indentation)
      (if (<= indentation-point (point))
	  '(0)
	(setq current-token (hs-indent-kb-peek-token))
	(catch 'parse-end
	  (hs-indent-kb-toplevel)
	  (when (not (equal current-token 'end-tokens))
	    (parse-error "Illegal token: %s" current-token)))
	possible-indentations))))

(defun hs-indent-kb-find-indentations ()
  (let ((ppss (syntax-ppss)))
    (cond
     ((nth 3 ppss) '(0))
     ((nth 4 ppss)
      (if (save-excursion
	    (and (skip-syntax-forward "-")
		 (eolp)
		 (not (> (forward-line 1) 0))
		 (not (nth 4 (syntax-ppss)))))
	  (hs-indent-kb-parse-to-indentations)
	'(0)))
     (t
      (hs-indent-kb-parse-to-indentations)))))

(defconst hs-indent-kb-toplevel-list
  '(("module" . hs-indent-kb-module)
    ("data" . hs-indent-kb-data)
    ("type" . hs-indent-kb-data)
    ("newtype" . hs-indent-kb-data)
    ("class" . hs-indent-kb-class-declaration)
    ("instance" . hs-indent-kb-class-declaration )))

(defconst hs-indent-kb-type-list
  '(("::"    . (lambda () (hs-indent-kb-statement-right #'hs-indent-kb-type)))
    ("("     . (lambda () (hs-indent-kb-list #'hs-indent-kb-type
						    ")" "," nil)))
    ("["     . (lambda () (hs-indent-kb-list #'hs-indent-kb-type
						    "]" "," nil)))
    ("{"     . (lambda () (hs-indent-kb-list #'hs-indent-kb-type
						    "}" "," nil)))))

(defconst hs-indent-kb-expression-list
  '(("data" . hs-indent-kb-data)
    ("type" . hs-indent-kb-data)
    ("newtype" . hs-indent-kb-data)
    ("if"    . (lambda () (hs-indent-kb-phrase
			   '(hs-indent-kb-expression
			     "then" hs-indent-kb-expression
			     "else" hs-indent-kb-expression))))
    ("let"   . (lambda () (hs-indent-kb-phrase
			   '(hs-indent-kb-declaration-layout
			     "in" hs-indent-kb-expression))))
    ("do"    . (lambda () (hs-indent-kb-with-starter
			   #'hs-indent-kb-expression-layout nil)))
    ("mdo"   . (lambda () (hs-indent-kb-with-starter
			   #'hs-indent-kb-expression-layout nil)))
    ("case"  . (lambda () (hs-indent-kb-phrase
			   '(hs-indent-kb-expression
			     "of" hs-indent-kb-case-layout))))
    ("\\"    . (lambda () (hs-indent-kb-phrase
			   '(hs-indent-kb-expression
			     "->" hs-indent-kb-expression))))
    ("proc"  . (lambda () (hs-indent-kb-phrase
			   '(hs-indent-kb-expression
			     "->" hs-indent-kb-expression))))
    ("where" . (lambda () (hs-indent-kb-with-starter
			   #'hs-indent-kb-declaration-layout nil t)))
    ("::"    . (lambda () (hs-indent-kb-statement-right #'hs-indent-kb-type)))
    ("="     . (lambda () (hs-indent-kb-statement-right #'hs-indent-kb-expression)))
    ("<-"    . (lambda () (hs-indent-kb-statement-right #'hs-indent-kb-expression)))
    ("("     . (lambda () (hs-indent-kb-list #'hs-indent-kb-expression
						    ")" '(list "," "->") nil)))
    ("["     . (lambda () (hs-indent-kb-list #'hs-indent-kb-expression
						    "]" "," "|")))
    ("{"     . (lambda () (hs-indent-kb-list #'hs-indent-kb-expression
						    "}" "," nil)))))
	  
(defun hs-indent-kb-expression-layout ()
  (hs-indent-kb-layout #'hs-indent-kb-expression))

(defun hs-indent-kb-declaration-layout ()
  (hs-indent-kb-layout #'hs-indent-kb-declaration))

(defun hs-indent-kb-case-layout ()
  (hs-indent-kb-layout #'hs-indent-kb-case))

(defun hs-indent-kb-fundep ()
  (hs-indent-kb-with-starter
   (lambda () (hs-indent-kb-separated
	       #'hs-indent-kb-fundep1 "," nil))
   nil))

(defun hs-indent-kb-fundep1 ()
  (let ((current-indent (haskell-current-column)))
    (while (member current-token '(value "->"))
      (hs-indent-kb-read-next-token))
    (when (and (equal current-token 'end-tokens)
	       (member following-token '(value "->")))
      (hs-indent-kb-add-indentation current-indent))))

(defun hs-indent-kb-toplevel ()
  (hs-indent-kb-layout
   (lambda ()
	 (let ((parser (assoc current-token hs-indent-kb-toplevel-list)))
	   (if parser
		   (funcall (cdr parser))
		 (hs-indent-kb-declaration))))))

(defun hs-indent-kb-type ()
  (let ((current-indent (haskell-current-column)))
    (catch 'return
      (while t
		(cond
		 ((member current-token '(value operator "->"))
		  (hs-indent-kb-read-next-token))

		 ((equal current-token 'end-tokens)
		  (when (member following-token
						'(value operator no-following-token
								"->" "(" "[" "{" "::"))
			(hs-indent-kb-add-indentation current-indent))
		  (throw 'return nil))
		 
		 (t (let ((parser (assoc current-token hs-indent-kb-type-list)))
			  (if (not parser)
				  (throw 'return nil)
				(funcall (cdr parser))))))))))

(defun hs-indent-kb-data ()
  (hs-indent-kb-with-starter
   (lambda ()
     (when (equal current-token "instance")
       (hs-indent-kb-read-next-token))
     (hs-indent-kb-type)
     (cond ((equal current-token "=")
	    (hs-indent-kb-with-starter
	     (lambda () (hs-indent-kb-separated #'hs-indent-kb-type "|" "deriving"))
	     nil))
	   ((equal current-token "where")
	    (hs-indent-kb-with-starter
	     #'hs-indent-kb-expression-layout nil))))
   nil))

(defun hs-indent-kb-class-declaration ()
  (hs-indent-kb-with-starter
   (lambda ()
     (hs-indent-kb-type)
     (when (equal current-token "|")
       (hs-indent-kb-fundep))
     (when (equal current-token "where")
       (hs-indent-kb-with-starter
	#'hs-indent-kb-expression-layout nil)))
   nil))

(defun hs-indent-kb-module ()
  (hs-indent-kb-with-starter
   (lambda ()
	 (let ((current-indent (haskell-current-column)))
	   (hs-indent-kb-read-next-token)
	   (when (equal current-token "(")
		 (hs-indent-kb-list
		  #'hs-indent-kb-module-export
		  ")" "," nil))
	   (when (equal current-token 'end-tokens)
		 (hs-indent-kb-add-indentation current-indent)
		 (throw 'parse-end nil))
	   (when (equal current-token "where")
		 (hs-indent-kb-read-next-token)
		 (when (equal current-token 'end-tokens)
		   (hs-indent-kb-add-layout-indent)
		   (throw 'parse-end nil))
		 (hs-indent-kb-layout #'hs-indent-kb-toplevel))))
   nil))

(defun hs-indent-kb-module-export ()
  (cond ((equal current-token "module")
		 (let ((current-indent (haskell-current-column)))
		   (hs-indent-kb-read-next-token)
		   (cond ((equal current-token 'end-tokens)
				  (hs-indent-kb-add-indentation current-indent))
				 ((equal current-token 'value)
				  (hs-indent-kb-read-next-token)))))
		(t (hs-indent-kb-type))))

(defun hs-indent-kb-list (parser end sep stmt-sep)
  (hs-indent-kb-with-starter
   `(lambda () (hs-indent-kb-separated #',parser
											  ,sep
											  ,stmt-sep))
   end))

(defun hs-indent-kb-with-starter (parser end &optional where-expr?)
  (let ((starter-column (haskell-current-column))
		(current-indent current-indent)
		(left-indent (if (= (haskell-current-column) (current-indentation))
						 (haskell-current-column) left-indent)))
    (hs-indent-kb-read-next-token)
    (when (equal current-token 'end-tokens)
      (if (equal following-token end)
	  (hs-indent-kb-add-indentation starter-column)
        (if where-expr?
            (hs-indent-kb-add-where-post-indent left-indent)
	  (hs-indent-kb-add-indentation
	   (+ left-indent hs-indent-kb-left-offset))))
      (throw 'parse-end nil))
    (let* ((current-indent (haskell-current-column))
		   (starter-indent (min starter-column current-indent))
		   (left-indent (if end (+ current-indent hs-indent-kb-starter-offset)
						  left-indent)))
      (funcall parser)
      (cond ((equal current-token 'end-tokens)
			 (when (equal following-token end)
			   (hs-indent-kb-add-indentation starter-indent))
			 (when end (throw 'parse-end nil))) ;; add no indentations
			((equal current-token end)
			 (hs-indent-kb-read-next-token)) ;; continue
			(end (parse-error "Illegal token: %s" current-token))))))

(defun hs-indent-kb-case ()
  (hs-indent-kb-expression)
  (cond ((equal current-token 'end-tokens)
	 (hs-indent-kb-add-indentation current-indent))
	((equal current-token "|")
	 (hs-indent-kb-with-starter
	  (lambda () (hs-indent-kb-separated #'hs-indent-kb-case "|" nil))
	  nil))
	((equal current-token "->")
	 (hs-indent-kb-statement-right #'hs-indent-kb-expression))
	;; otherwise fallthrough
	))

(defun hs-indent-kb-statement-right (parser)
    (hs-indent-kb-read-next-token)
    (when (equal current-token 'end-tokens)
      (hs-indent-kb-add-indentation
       (+ left-indent hs-indent-kb-left-offset))
      (throw 'parse-end nil))
    (let ((current-indent (haskell-current-column)))
	  (funcall parser)))

(defun hs-indent-kb-simple-declaration ()
  (hs-indent-kb-expression)
  (cond ((equal current-token "=")
	 (hs-indent-kb-statement-right #'hs-indent-kb-expression))
	((equal current-token "::")
	 (hs-indent-kb-statement-right #'hs-indent-kb-type))
	((and (equal current-token 'end-tokens)
	      (equal following-token "="))
	 (hs-indent-kb-add-indentation current-indent)
	 (throw 'parse-end nil))))

(defun hs-indent-kb-declaration ()
  (hs-indent-kb-expression)
  (cond ((equal current-token "|")
	 (hs-indent-kb-with-starter
	  (lambda () (hs-indent-kb-separated #'hs-indent-kb-expression "," "|"))
	  nil))
	((equal current-token 'end-tokens)
	 (when (member following-token '("|" "=" "::" ","))
	   (hs-indent-kb-add-indentation current-indent)
	   (throw 'parse-end nil)))))

(defun hs-indent-kb-layout (parser)
  (if (equal current-token "{")
      (hs-indent-kb-list parser "}" ";" nil)
    (hs-indent-kb-implicit-layout-list parser)))

(defun hs-indent-kb-expression-token (token)
  (member token '("if" "let" "do" "case" "\\" "(" "[" "::"
		  value operator no-following-token)))

(defun hs-indent-kb-expression ()
  (let ((current-indent (haskell-current-column)))
    (catch 'return
      (while t
	(cond
	 ((member current-token '(value operator))
	  (hs-indent-kb-read-next-token))

	 ((equal current-token 'end-tokens)
	  (cond ((equal following-token "where")
		 (hs-indent-kb-add-where-pre-indent))
		((hs-indent-kb-expression-token following-token)
		 (hs-indent-kb-add-indentation
		  current-indent)))
	  (throw 'return nil))

	 (t (let ((parser (assoc current-token hs-indent-kb-expression-list)))
	      (when (null parser)
		(throw 'return nil))
	      (funcall (cdr parser))
	      (when (and (equal current-token 'end-tokens)
			 (equal (car parser) "let")
			 (= hs-indent-kb-layout-offset current-indent)
			 (hs-indent-kb-expression-token following-token))
		;; inside a layout, after a let construct
		(hs-indent-kb-add-layout-indent)
		(throw 'parse-end nil))
	      (unless (member (car parser) '("(" "[" "{" "do" "case"))
		(throw 'return nil)))))))))

(defun hs-indent-kb-test-indentations ()
  (interactive)
  (let ((indentations (save-excursion (hs-indent-kb-find-indentations)))
	(str "")
	(pos 0))
    (while indentations
      (when (>= (car indentations) pos)
	(setq str (concat str (make-string (- (car indentations) pos) ?\ )
			  "|"))
	(setq pos (+ 1 (car indentations))))
      (setq indentations (cdr indentations)))
    (end-of-line)
    (newline)
    (insert str)))

(defun hs-indent-kb-separated (parser separator stmt-separator)
  (catch 'return
    (while t
      (funcall parser)
      (cond ((if (listp separator) (member current-token separator) (equal current-token separator))
	     (hs-indent-kb-at-separator))

	    ((equal current-token stmt-separator)
	     (setq starter-indent (haskell-current-column))
	     (hs-indent-kb-at-separator))

	    ((equal current-token 'end-tokens)
	     (cond ((or (equal following-token separator)
			(equal following-token stmt-separator))
		    (hs-indent-kb-add-indentation starter-indent)
		    (throw 'parse-end nil)))
	     (throw 'return nil))

	    (t (throw 'return nil))))))

(defun hs-indent-kb-at-separator ()
  (let ((separator-column
	 (and (= (haskell-current-column) (current-indentation))
	      (haskell-current-column))))
    (hs-indent-kb-read-next-token)
    (cond ((eq current-token 'end-tokens)
	   (hs-indent-kb-add-indentation current-indent)
	   (throw 'return nil))
	  (separator-column ;; on the beginning of the line
	   (setq current-indent (haskell-current-column))
	   (setq starter-indent separator-column)))))

(defun hs-indent-kb-implicit-layout-list (parser)
  (let* ((layout-indent (haskell-current-column))
		 (current-indent (haskell-current-column))
		 (left-indent (haskell-current-column)))
    (catch 'return
      (while t
	(let ((left-indent left-indent))
	  (funcall parser))
	(cond ((member current-token '(layout-next ";"))
	       (hs-indent-kb-read-next-token))
	      ((equal current-token 'end-tokens)
	       (when (or (hs-indent-kb-expression-token following-token)
					 (equal following-token ";"))
			 (hs-indent-kb-add-layout-indent))
	       (throw 'return nil))
	      (t (throw 'return nil))))))
  ;; put hs-indent-kb-read-next-token outside the current-indent definition
  ;; so it will not return 'layout-end again
  (when (eq current-token 'layout-end)
    (hs-indent-kb-read-next-token))) ;; leave layout at 'layout-end or illegal token

(defun hs-indent-kb-phrase (phrase)
  (hs-indent-kb-with-starter
   `(lambda () (hs-indent-kb-phrase-rest ',phrase))
   nil))

(defun hs-indent-kb-phrase-rest (phrase)
  (let ((starter-line parse-line-number))
    (let ((current-indent (haskell-current-column)))
      (funcall (car phrase)))
    (cond
     ((equal current-token 'end-tokens)
      (cond ((null (cdr phrase))) ;; fallthrough
	    ((equal following-token (cadr phrase))
	     (hs-indent-kb-add-indentation starter-indent)
	     (throw 'parse-end nil))
	    ((equal (cadr phrase) "in")
	     (when (= left-indent layout-indent)
	       (hs-indent-kb-add-layout-indent)
	       (throw 'parse-end nil)))
	    (t (throw 'parse-end nil))))

     ((null (cdr phrase)))
     
     ((equal (cadr phrase) current-token)
      (let* ((on-new-line (= (haskell-current-column) (current-indentation)))
	     (lines-between (- parse-line-number starter-line))
	     (left-indent (if (<= lines-between 0)
			      left-indent
			    starter-indent)))
	(hs-indent-kb-read-next-token)
	(when (equal current-token 'end-tokens)
	  (hs-indent-kb-add-indentation
	   (cond ((member (cadr phrase) '("then" "else"))
		  (+ starter-indent hs-indent-kb-ifte-offset))
		 ((member (cadr phrase) '("in" "->"))
		  ;; expression ending in another expression
		  (if on-new-line
		      (+ left-indent hs-indent-kb-starter-offset)
		    left-indent))
		 (t (+ left-indent hs-indent-kb-left-offset))))
	  (throw 'parse-end nil))
	(hs-indent-kb-phrase-rest (cddr phrase))))

     ((equal (cadr phrase) "in")) ;; fallthrough
     (t (parse-error "Expecting %s" (cadr phrase))))))

(defun hs-indent-kb-add-indentation (indent)
  (hs-indent-kb-push-indentation
   (if (<= indent layout-indent)
       (+ layout-indent hs-indent-kb-layout-offset)
     indent)))

(defun hs-indent-kb-add-layout-indent ()
  (hs-indent-kb-push-indentation layout-indent))

(defun hs-indent-kb-add-where-pre-indent ()
  (hs-indent-kb-push-indentation
   (+ layout-indent hs-indent-kb-where-pre-offset))
  (if (= layout-indent hs-indent-kb-layout-offset)
      (hs-indent-kb-push-indentation
       hs-indent-kb-where-pre-offset)))

(defun hs-indent-kb-add-where-post-indent (indent)
  (hs-indent-kb-push-indentation
   (+ indent hs-indent-kb-where-post-offset)))

(defun hs-indent-kb-push-indentation (indent)
  (when (or (null possible-indentations)
	    (< indent (car possible-indentations)))
    (setq possible-indentations
	  (cons indent possible-indentations))))

(defun hs-indent-kb-token-test ()
  (let ((current-token nil)
	(following-token nil)
	(layout-indent 0)
	(indentation-point (mark)))
    (hs-indent-kb-read-next-token)))

(defun hs-indent-kb-read-next-token ()
  (cond ((eq current-token 'end-tokens)
	 'end-tokens)
	((eq current-token 'layout-end)
	 (cond ((> layout-indent (haskell-current-column))
		'layout-end)
	       ((= layout-indent (haskell-current-column))
		(setq current-token 'layout-next))
	       ((< layout-indent (haskell-current-column))
		(setq current-token (hs-indent-kb-peek-token)))))
	((eq current-token 'layout-next)
	 (setq current-token (hs-indent-kb-peek-token)))
	((> layout-indent (haskell-current-column))
	 (setq current-token 'layout-end))
	(t
	 (hs-indent-kb-skip-token)
	 (if (>= (point) indentation-point)
	     (progn
	       (setq following-token
		     (if (= (point) indentation-point)
			 (hs-indent-kb-peek-token)
		       'no-following-token))
	       (setq current-token 'end-tokens))
	   (when (= (haskell-current-column) (current-indentation))
	     ;; on a new line
	     (setq current-indent (haskell-current-column))
	     (setq left-indent (haskell-current-column))
	     (setq parse-line-number (+ parse-line-number 1)))
	   (cond ((> layout-indent (haskell-current-column))
		  (setq current-token 'layout-end))
		 ((= layout-indent (haskell-current-column))
		  (setq current-token 'layout-next))
		 (t (setq current-token (hs-indent-kb-peek-token))))))))

(defun hs-indent-kb-peek-token ()
  (cond ((looking-at "\\(if\\|then\\|else\\|let\\|in\\|mdo\\|do\\|proc\\|case\\|of\\|where\\|module\\|deriving\\|data\\|type\\|newtype\\|class\\|instance\\)\\([^[:alpha:]'_]\\|$\\)")
	 (match-string 1))
	((looking-at "[][(){}[,;]")
	 (match-string 0))
	((looking-at "\\(\\\\\\|->\\|<-\\|::\\|=\\||\\)\\([^-:!#$%&*+./<=>?@\\\\^|~]\\|$\\)")
	 (match-string 1))
	((looking-at"[-:!#$%&*+./<=>?@\\\\^|~`]" )
	 'operator)
	(t 'value)))

(defun hs-indent-kb-skip-token ()
  "Skip to the next token."
  (let ((case-fold-search nil))
    (if (or (looking-at "'\\([^\\']\\|\\\\.\\)*'")
            (looking-at "\"\\([^\\\"]\\|\\\\.\\)*\"")
            (looking-at	; Hierarchical names always start with uppercase
             "[[:upper:]]\\(\\sw\\|'\\)*\\(\\.\\(\\sw\\|'\\)+\\)*")
            (looking-at "\\sw\\(\\sw\\|'\\)*") ; Only unqualified vars can start with lowercase
            (looking-at "[0-9][0-9oOxXeE+-]*")
            (looking-at "[-:!#$%&*+./<=>?@\\\\^|~]+")
            (looking-at "[](){}[,;]")
            (looking-at "`[[:alnum:]']*`"))
        (goto-char (match-end 0))
    ;; otherwise skip until space found
      (skip-syntax-forward "^-"))
    (forward-comment (buffer-size))))

(provide 'hs-indent-kb)
;;; hs-indent-kb.el ends here
