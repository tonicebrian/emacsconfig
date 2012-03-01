;;; hs-indent-glfsf.el --- "semi-intelligent" indentation module for Haskell Mode

;; Copyright 2004, 2005, 2007, 2008, 2009  Free Software Foundation, Inc.
;; Copyright 1997-1998  Guy Lapalme

;; Author: 1997-1998 Guy Lapalme <lapalme@iro.umontreal.ca>

;; Keywords: indentation Haskell layout-rule
;; Version: 1.2
;; URL: http://www.iro.umontreal.ca/~lapalme/layout/index.html

;;; This file is not part of GNU Emacs.

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


;;; Commentary:

;; Purpose:
;;
;; To support automatic indentation of Haskell programs using
;; the layout rule described in section 1.5 and appendix B.3 of the
;; the Haskell report.  The rationale and the implementation principles
;; are described in an article to appear in Journal of Functional Programming.
;;   "Dynamic tabbing for automatic indentation with the layout rule"
;;
;; It supports literate scripts.
;; Haskell indentation is performed
;;     within \begin{code}...\end{code} sections of a literate script
;;     and in lines beginning with > with Bird style literate script
;; TAB aligns to the left column outside of these sections.
;;
;; Customisation:
;;       The "standard" offset for statements is 4 spaces.
;;       It can be changed by setting the variable "hs-indent-glfsf-offset" to
;;       another value
;;
;;       The default number of blanks after > in a Bird style literate script
;;       is 1; it can be changed by setting the variable
;;       "hs-indent-glfsf-literate-Bird-default-offset"
;;
;;       `hs-indent-glfsf-hook' is invoked if not nil.
;;
;; All functions/variables start with
;; `(turn-(on/off)-)hs-indent-glfsf' or `hs-indent-glfsf-'.

;; This file can also be used as a hook for the Hugs Mode developed by
;;         Chris Van Humbeeck <chris.vanhumbeeck@cs.kuleuven.ac.be>
;; It can be obtained at:
;; http://www-i2.informatik.rwth-aachen.de/Forschung/FP/Haskell/hugs-mode.el
;;
;; For the Hugs mode put the following in your .emacs
;;
;;(setq auto-mode-alist (append auto-mode-alist '(("\\.hs\\'" . hugs-mode))))
;;(autoload 'hugs-mode "hugs-mode" "Go into hugs mode" t)
;;
;; If only the indentation mode is used then replace the two
;; preceding lines with
;;(setq auto-mode-alist (append auto-mode-alist
;;                              '(("\\.hs\\'" . turn-on-hs-indent-glfsf))))
;;(autoload 'turn-on-hs-indent-glfsf "hindent" "Indentation mode for Haskell" t)
;;
;; For indentation in both cases then add the following to your .emacs
;;(add-hook 'hugs-mode-hook 'turn-on-hs-indent-glfsf)
;;(autoload 'hs-indent-glfsf-cycle "hindent" "Indentation cycle for Haskell" t)
;;

;;; Code:

(eval-when-compile (require 'cl))	;need defs of push and pop
(defvar hs-mode-literate nil)

(defgroup hs-indent-glfsf nil
  "Haskell indentation."
  :group 'haskell
  :prefix "hs-indent-glfsf-")

(defcustom hs-indent-glfsf-offset 4
  "Indentation of Haskell statements with respect to containing block."
  :type 'integer
  :group 'hs-indent-glfsf)

(defcustom hs-indent-glfsf-literate-Bird-default-offset 1
  "Default number of blanks after > in a Bird style literate script."
  :type 'integer
  :group 'hs-indent-glfsf)

(defcustom hs-indent-glfsf-rhs-align-column 0
  "Column on which to align right-hand sides (use 0 for ad-hoc alignment)."
  :type 'integer
  :group 'hs-indent-glfsf)

(defun hs-indent-glfsf-point-to-col (apoint)
  "Return the column number of APOINT."
  (save-excursion
    (goto-char apoint)
    (current-column)))

(defconst hs-indent-glfsf-start-keywords-re
  (concat "\\<"
          (regexp-opt '("class" "data" "import" "infix" "infixl" "infixr"
                        "instance" "module" "newtype" "primitive" "type") t)
          "\\>")
  "Regexp for keywords to complete when standing at the first word of a line.")


;; Customizations for different kinds of environments
;; in which dealing with low-level events are different.
(defun hs-indent-glfsf-mark-active ()
  (if (featurep 'xemacs)
      (if zmacs-regions
          zmacs-region-active-p
        t)
    mark-active))

;;  for pushing indentation information

(defvar hs-indent-glfsf-info)            ;Used with dynamic scoping.

(defun hs-indent-glfsf-push-col (col &optional name)
  "Push indentation information for the column COL.
The info is followed by NAME (if present).
Makes sure that the same indentation info is not pushed twice.
Uses free var `hs-indent-glfsf-info'."
  (let ((tmp (cons col name)))
    (if (member tmp hs-indent-glfsf-info)
	hs-indent-glfsf-info
      (push tmp hs-indent-glfsf-info))))

(defun hs-indent-glfsf-push-pos (pos &optional name)
  "Push indentation information for POS followed by NAME (if present)."
  (hs-indent-glfsf-push-col (hs-indent-glfsf-point-to-col pos) name))

;; (defvar hs-indent-glfsf-tab-align nil
;;   "Align all indentations on TAB stops.")

(defun hs-indent-glfsf-column+offset (column offset)
  (unless offset (setq offset hs-indent-glfsf-offset))
  (setq column (+ column offset))
  ;; (if (and hs-indent-glfsf-tab-align (> offset 0))
  ;;     (* 8 (/ (+ column 7) 8))
    column) ;; )

(defun hs-indent-glfsf-push-pos-offset (pos &optional offset)
  "Pushes indentation information for the column corresponding to POS
followed by an OFFSET (if present use its value otherwise use
`hs-indent-glfsf-offset')."
  (hs-indent-glfsf-push-col (hs-indent-glfsf-column+offset
                            (hs-indent-glfsf-point-to-col pos)
                            offset)))

;; redefinition of some Emacs function for dealing with
;; Bird Style literate scripts

(defun hs-indent-glfsf-bolp ()
  "`bolp' but dealing with Bird-style literate scripts."
  (or (bolp)
      (and (eq hs-mode-literate 'bird)
           (<= (current-column) (1+ hs-indent-glfsf-literate-Bird-default-offset))
           (eq (char-after (line-beginning-position)) ?\>))))

(defun hs-indent-glfsf-empty-line-p ()
  "Checks if the current line is empty; deals with Bird style scripts."
  (save-excursion
    (beginning-of-line)
    (if (and (eq hs-mode-literate 'bird)
             (eq (following-char) ?\>))
        (forward-char 1))
    (looking-at "[ \t]*$")))

(defun hs-indent-glfsf-back-to-indentation ()
  "`back-to-indentation' function but dealing with Bird-style literate scripts."
  (if (and (eq hs-mode-literate 'bird)
           (progn (beginning-of-line) (eq (following-char) ?\>)))
      (progn
        (forward-char 1)
        (skip-chars-forward " \t"))
    (back-to-indentation)))

(defun hs-indent-glfsf-current-indentation ()
  "`current-indentation' function dealing with Bird-style literate scripts."
  (if (eq hs-mode-literate 'bird)
      (save-excursion
        (hs-indent-glfsf-back-to-indentation)
        (current-column))
    (current-indentation)))

(defun hs-indent-glfsf-backward-to-indentation (n)
  "`backward-to-indentation' function dealing with Bird-style literate scripts."
  (if (eq hs-mode-literate 'bird)
      (progn
        (forward-line (- n))
        (hs-indent-glfsf-back-to-indentation))
    (backward-to-indentation n)))

(defun hs-indent-glfsf-forward-line (&optional n)
  "`forward-line' function but dealing with Bird-style literate scripts."
  (prog1
      (forward-line n)
    (if (and (eq hs-mode-literate 'bird) (eq (following-char) ?\>))
        (progn (forward-char 1)                ; skip > and initial blanks...
               (skip-chars-forward " \t")))))

(defun hs-indent-glfsf-line-to (n)
  "`indent-line-to' function but dealing with Bird-style literate scripts."
  (if (eq hs-mode-literate 'bird)
      (progn
        (beginning-of-line)
        (if (eq (following-char) ?\>)
            (delete-char 1))
        (delete-horizontal-space)       ; remove any starting TABs so
        (indent-line-to n)              ; that indent-line only adds spaces
        (save-excursion
          (beginning-of-line)
          (if (> n 0) (delete-char 1))  ; delete the first space before
          (insert ?\>)))                ; inserting a >
    (indent-line-to n)))

(defun hs-indent-glfsf-skip-blanks-and-newlines-forward (end)
  "Skip forward blanks, tabs and newlines until END.
Take account of Bird-style literate scripts."
  (skip-chars-forward " \t\n" end)
  (if (eq hs-mode-literate 'bird)
      (while (and (bolp) (eq (following-char) ?\>))
        (forward-char 1)                ; skip >
        (skip-chars-forward " \t\n" end))))

(defun hs-indent-glfsf-skip-blanks-and-newlines-backward (start)
  "Skip backward blanks, tabs and newlines up to START.
Take account of Bird-style literate scripts."
  (skip-chars-backward " \t\n" start)
  (if (eq hs-mode-literate 'bird)
      (while (and (eq (current-column) 1)
                  (eq (preceding-char) ?\>))
        (forward-char -1)               ; skip back >
        (skip-chars-backward " \t\n" start))))

;; specific functions for literate code

(defun hs-indent-glfsf-within-literate-code ()
  "Check if point is within a part of literate Haskell code.
If so, return its start; otherwise return nil:
If it is Bird-style, then return the position of the >;
otherwise return the ending position of \\begin{code}."
  (save-excursion
    (case hs-mode-literate
      (bird
       (beginning-of-line)
       (if (or (eq (following-char) ?\>)
               (and (bolp) (forward-line -1) (eq (following-char) ?\>)))
           (progn
             (while (and (zerop (forward-line -1))
                         (eq (following-char) ?\>)))
             (if (not (eq (following-char) ?\>))
                 (forward-line))
             (point))))
      ;;  Look for a \begin{code} or \end{code} line.
      ((latex tex)
       (if (re-search-backward
            "^\\(\\\\begin{code}$\\)\\|\\(\\\\end{code}$\\)" nil t)
           ;; within a literate code part if it was a \\begin{code}.
           (match-end 1)))
      (t (error "hs-indent-glfsf-within-literate-code: should not happen!")))))

(defun hs-indent-glfsf-put-region-in-literate (beg end &optional arg)
  "Put lines of the region as a piece of literate code.
With prefix arg, remove indication that the region is literate code.
It deals with both Bird style and non Bird-style scripts."
  (interactive "r\nP")
  (unless hs-mode-literate
    (error "Cannot put a region in literate in a non literate script"))
  (if (eq hs-mode-literate 'bird)
      (let ((comment-start "> ")        ; Change dynamic bindings for
            (comment-start-skip "^> ?") ; comment-region.
            (comment-end "")
            (comment-end-skip "\n")
            (comment-style 'plain))
        (comment-region beg end arg))
    ;; Not Bird style.
    (if arg                             ; Remove the literate indication.
        (save-excursion
          (goto-char end)               ; Remove end.
          (if (re-search-backward "^\\\\end{code}[ \t\n]*\\="
                                  (line-beginning-position -2) t)
              (delete-region (point) (line-beginning-position 2)))
          (goto-char beg)               ; Remove end.
          (beginning-of-line)
          (if (looking-at "\\\\begin{code}")
              (kill-line 1)))
      (save-excursion                   ; Add the literate indication.
        (goto-char end)
        (unless (bolp) (newline))
        (insert "\\end{code}\n")
        (goto-char beg)
        (unless (bolp) (newline))
        (insert "\\begin{code}\n")))))

;;; Start of indentation code

(defcustom hs-indent-glfsf-look-past-empty-line t
  "If nil, indentation engine will not look past an empty line for layout points."
  :group 'hs-indent-glfsf
  :type 'boolean)

(defun hs-indent-glfsf-start-of-def ()
  "Return the position of the start of a definition.
The start of a def is expected to be recognizable by starting in column 0,
unless `hs-indent-glfsf-look-past-empty-line' is nil, in which case we
take a coarser approximation and stop at the first empty line."
  (save-excursion
    (let ((start-code (and hs-mode-literate
                           (hs-indent-glfsf-within-literate-code)))
          (top-col (if (eq hs-mode-literate 'bird) 2 0))
          (save-point (point)))
      ;; determine the starting point of the current piece of code
      (setq start-code (if start-code (1+ start-code) (point-min)))
      ;; go backward until the first preceding empty line
      (hs-indent-glfsf-forward-line -1)
      (while (and (if hs-indent-glfsf-look-past-empty-line
                      (or (> (hs-indent-glfsf-current-indentation) top-col)
                          (hs-indent-glfsf-empty-line-p))
                    (and (> (hs-indent-glfsf-current-indentation) top-col)
                         (not (hs-indent-glfsf-empty-line-p))))
                  (> (point) start-code)
                  (= 0 (hs-indent-glfsf-forward-line -1))))
      ;; go forward after the empty line
      (if (hs-indent-glfsf-empty-line-p)
          (hs-indent-glfsf-forward-line 1))
      (setq start-code (point))
      ;; find the first line of code which is not a comment
      (forward-comment (point-max))
      (if (> (point) save-point)
	  start-code
	(point)))))

(defun hs-indent-glfsf-open-structure (start end)
  "If any structure (list or tuple) is not closed, between START and END,
returns the location of the opening symbol, nil otherwise."
  (save-excursion
    (nth 1 (parse-partial-sexp start end))))

(defun hs-indent-glfsf-in-string (start end)
  "If a string is not closed , between START and END, returns the
location of the opening symbol, nil otherwise."
  (save-excursion
    (let ((pps (parse-partial-sexp start end)))
      (if (nth 3 pps) (nth 8 pps)))))

(defun hs-indent-glfsf-in-comment (start end)
  "Check, starting from START, if END is at or within a comment.
Returns the location of the start of the comment, nil otherwise."
  (let (pps)
    (assert (<= start end))
    (cond ((= start end) nil)
	  ((nth 4 (save-excursion (setq pps (parse-partial-sexp start end))))
	   (nth 8 pps))
	  ;; We also want to say that we are *at* the beginning of a comment.
	  ((and (not (nth 8 pps))
                (>= (point-max) (+ end 2))
		(nth 4 (save-excursion
			 (setq pps (parse-partial-sexp end (+ end 2))))))
	   (nth 8 pps)))))

(defvar hs-indent-glfsf-off-side-keywords-re
      "\\<\\(do\\|let\\|of\\|where\\)\\>[ \t]*")

(defun hs-indent-glfsf-type-at-point ()
  "Return the type of the line (also puts information in `match-data')."
  (cond
   ((hs-indent-glfsf-empty-line-p) 'empty)
   ((hs-indent-glfsf-in-comment (point-min) (point)) 'comment)
   ((looking-at "\\(\\([[:alpha:]]\\(\\sw\\|'\\)*\\)\\|_\\)[ \t\n]*")
    'ident)
   ((looking-at "\\(|[^|]\\)[ \t\n]*") 'guard)
   ((looking-at "\\(=[^>=]\\|::\\|->\\|<-\\)[ \t\n]*") 'rhs)
   (t 'other)))

(defvar hs-indent-glfsf-current-line-first-ident ""
  "Global variable that keeps track of the first ident of the line to indent.")


(defun hs-indent-glfsf-contour-line (start end)
  "Generate contour information between START and END points."
  (if (< start end)
      (save-excursion
	(goto-char end)
	(hs-indent-glfsf-skip-blanks-and-newlines-backward start)
        (let ((cur-col (current-column))            ; maximum column number
              (fl 0)     ; number of lines that forward-line could not advance
              contour)
          (while (and (> cur-col 0) (= fl 0) (>= (point) start))
            (hs-indent-glfsf-back-to-indentation)
	    (if (< (point) start) (goto-char start))
            (and (not (member (hs-indent-glfsf-type-at-point)
                              '(empty comment))) ; skip empty and comment lines
                 (< (current-column) cur-col) ; less indented column found
                 (push (point) contour) ; new contour point found
                 (setq cur-col (current-column)))
            (setq fl (hs-indent-glfsf-forward-line -1)))
          contour))))

(defun hs-indent-glfsf-next-symbol (end)
  "Move point to the next symbol."
  (skip-syntax-forward ")" end)
  (if (< (point) end)
     (progn
       (forward-sexp 1)
       (hs-indent-glfsf-skip-blanks-and-newlines-forward end))))

(defun hs-indent-glfsf-next-symbol-safe (end)
  "Puts point to the next following symbol, or to end if there are no more symbols in the sexp."
  (condition-case errlist (hs-indent-glfsf-next-symbol end)
      (error (goto-char end))))

(defun hs-indent-glfsf-separate-valdef (start end)
  "Return a list of positions for important parts of a valdef."
  (save-excursion
    (let (valname valname-string aft-valname
                  guard aft-guard
                  rhs-sign aft-rhs-sign
                  type)
      ;; "parse" a valdef separating important parts
      (goto-char start)
      (setq type (hs-indent-glfsf-type-at-point))
      (if (or (memq type '(ident other))) ; possible start of a value def
          (progn
            (if (eq type 'ident)
                (progn
                  (setq valname (match-beginning 0))
                  (setq valname-string (match-string 0))
                  (goto-char (match-end 0)))
              (skip-chars-forward " \t" end)
              (setq valname (point))    ; type = other
              (hs-indent-glfsf-next-symbol-safe end))
            (while (and (< (point) end)
                        (setq type (hs-indent-glfsf-type-at-point))
                        (or (memq type '(ident other))))
              (if (null aft-valname)
                  (setq aft-valname (point)))
              (hs-indent-glfsf-next-symbol-safe end))))
      (if (and (< (point) end) (eq type 'guard)) ; start of a guard
          (progn
            (setq guard (match-beginning 0))
            (goto-char (match-end 0))
            (while (and (< (point) end)
                        (setq type (hs-indent-glfsf-type-at-point))
                        (not (eq type 'rhs)))
              (if (null aft-guard)
                  (setq aft-guard (point)))
              (hs-indent-glfsf-next-symbol-safe end))))
      (if (and (< (point) end) (eq type 'rhs)) ; start of a rhs
          (progn
            (setq rhs-sign (match-beginning 0))
            (goto-char (match-end 0))
            (if (< (point) end)
                (setq aft-rhs-sign (point)))))
      (list valname valname-string aft-valname
            guard aft-guard rhs-sign aft-rhs-sign))))

(defsubst hs-indent-glfsf-no-otherwise (guard)
  "Check if there is no otherwise at GUARD."
  (save-excursion
    (goto-char guard)
    (not (looking-at "|[ \t]*otherwise\\>"))))


(defun hs-indent-glfsf-guard (start end end-visible indent-info)
  "Find indentation information for a line starting with a guard."
  (save-excursion
    (let* ((hs-indent-glfsf-info indent-info)
           (sep (hs-indent-glfsf-separate-valdef start end))
           (valname (nth 0 sep))
           (guard (nth 3 sep))
           (rhs-sign (nth 5 sep)))
      ;; push information indentation for the visible part
      (if (and guard (< guard end-visible) (hs-indent-glfsf-no-otherwise guard))
          (hs-indent-glfsf-push-pos guard)
        (if rhs-sign
            (hs-indent-glfsf-push-pos rhs-sign) ; probably within a data definition...
          (if valname
              (hs-indent-glfsf-push-pos-offset valname))))
      hs-indent-glfsf-info)))

(defun hs-indent-glfsf-rhs (start end end-visible indent-info)
  "Find indentation information for a line starting with a rhs."
  (save-excursion
    (let* ((hs-indent-glfsf-info indent-info)
           (sep (hs-indent-glfsf-separate-valdef start end))
           (valname (nth 0 sep))
           (guard (nth 3 sep))
           (rhs-sign (nth 5 sep)))
      ;; push information indentation for the visible part
      (if (and rhs-sign (< rhs-sign end-visible))
          (hs-indent-glfsf-push-pos rhs-sign)
        (if (and guard (< guard end-visible))
            (hs-indent-glfsf-push-pos-offset guard)
          (if valname                   ; always visible !!
              (hs-indent-glfsf-push-pos-offset valname))))
      hs-indent-glfsf-info)))

(defconst hs-indent-glfsf-decision-table
  (let ((or "\\)\\|\\("))
    (concat "\\("
            "1.1.11" or                 ; 1= vn gd rh arh
            "1.1.10" or                 ; 2= vn gd rh
            "1.1100" or                 ; 3= vn gd agd
            "1.1000" or                 ; 4= vn gd
            "1.0011" or                 ; 5= vn rh arh
            "1.0010" or                 ; 6= vn rh
            "110000" or                 ; 7= vn avn
            "100000" or                 ; 8= vn
            "001.11" or                 ; 9= gd rh arh
            "001.10" or                 ;10= gd rh
            "001100" or                 ;11= gd agd
            "001000" or                 ;12= gd
            "000011" or                 ;13= rh arh
            "000010" or                 ;14= rh
            "000000"                    ;15=
            "\\)")))

(defun hs-indent-glfsf-find-case (test)
  "Find the index that matches TEST in the decision table."
  (if (string-match hs-indent-glfsf-decision-table test)
      ;; use the fact that the resulting match-data is a list of the form
      ;; (0 6 [2*(n-1) nil] 0 6) where n is the number of the matching regexp
      ;; so n= ((length match-data)/2)-1
      (- (/ (length (match-data 'integers)) 2) 1)
    (error "hs-indent-glfsf-find-case: impossible case: %s" test)))

(defun hs-indent-glfsf-empty (start end end-visible indent-info)
  "Find indentation points for an empty line."
  (save-excursion
    (let* ((hs-indent-glfsf-info indent-info)
           (sep (hs-indent-glfsf-separate-valdef start end))
           (valname (pop sep))
           (valname-string (pop sep))
           (aft-valname (pop sep))
           (guard (pop sep))
           (aft-guard (pop sep))
           (rhs-sign (pop sep))
           (aft-rhs-sign (pop sep))
           (last-line (= end end-visible))
           (test (string
                  (if valname ?1 ?0)
                  (if (and aft-valname (< aft-valname end-visible)) ?1 ?0)
                  (if (and guard (< guard end-visible)) ?1 ?0)
                  (if (and aft-guard (< aft-guard end-visible)) ?1 ?0)
                  (if (and rhs-sign (< rhs-sign end-visible)) ?1 ?0)
                  (if (and aft-rhs-sign (< aft-rhs-sign end-visible)) ?1 ?0))))
      (if (and valname-string           ; special case for start keywords
               (string-match hs-indent-glfsf-start-keywords-re valname-string))
          (progn
            (hs-indent-glfsf-push-pos valname)
            ;; very special for data keyword
            (if (string-match "\\<data\\>" valname-string)
                (if rhs-sign (hs-indent-glfsf-push-pos rhs-sign)
                  (hs-indent-glfsf-push-pos-offset valname))
              (hs-indent-glfsf-push-pos-offset valname)))
        (case                           ; general case
            (hs-indent-glfsf-find-case test)
          ;; "1.1.11"   1= vn gd rh arh
          (1 (hs-indent-glfsf-push-pos valname)
             (hs-indent-glfsf-push-pos valname valname-string)
             (if (hs-indent-glfsf-no-otherwise guard) (hs-indent-glfsf-push-pos guard "| "))
             (hs-indent-glfsf-push-pos aft-rhs-sign))
          ;; "1.1.10"   2= vn gd rh
          (2 (hs-indent-glfsf-push-pos valname)
             (hs-indent-glfsf-push-pos valname valname-string)
             (if last-line
                 (hs-indent-glfsf-push-pos-offset guard)
               (if (hs-indent-glfsf-no-otherwise guard) (hs-indent-glfsf-push-pos guard "| "))))
          ;; "1.1100"   3= vn gd agd
          (3 (hs-indent-glfsf-push-pos valname)
             (hs-indent-glfsf-push-pos aft-guard)
             (if last-line (hs-indent-glfsf-push-pos-offset valname)))
          ;; "1.1000"   4= vn gd
          (4 (hs-indent-glfsf-push-pos valname)
             (if last-line (hs-indent-glfsf-push-pos-offset guard 2)))
          ;; "1.0011"   5= vn rh arh
          (5 (hs-indent-glfsf-push-pos valname)
             (if (or (and aft-valname (= (char-after rhs-sign) ?\=))
                     (= (char-after rhs-sign) ?\:))
                 (hs-indent-glfsf-push-pos valname valname-string))
             (hs-indent-glfsf-push-pos aft-rhs-sign))
          ;; "1.0010"   6= vn rh
          (6 (hs-indent-glfsf-push-pos valname)
             (hs-indent-glfsf-push-pos valname valname-string)
             (if last-line (hs-indent-glfsf-push-pos-offset valname)))
          ;; "110000"   7= vn avn
          (7 (hs-indent-glfsf-push-pos valname)
             (if last-line
                 (hs-indent-glfsf-push-pos aft-valname)
               (hs-indent-glfsf-push-pos valname valname-string)))
          ;; "100000"   8= vn
          (8 (hs-indent-glfsf-push-pos valname))
          ;; "001.11"   9= gd rh arh
          (9 (if (hs-indent-glfsf-no-otherwise guard) (hs-indent-glfsf-push-pos guard "| "))
             (hs-indent-glfsf-push-pos aft-rhs-sign))
          ;; "001.10"  10= gd rh
          (10 (if (hs-indent-glfsf-no-otherwise guard) (hs-indent-glfsf-push-pos guard "| "))
	      (if last-line (hs-indent-glfsf-push-pos-offset guard)))
          ;; "001100"  11= gd agd
          (11 (if (hs-indent-glfsf-no-otherwise guard) (hs-indent-glfsf-push-pos guard "| "))
	      (hs-indent-glfsf-push-pos aft-guard))
          ;; "001000"  12= gd
          (12 (if (hs-indent-glfsf-no-otherwise guard) (hs-indent-glfsf-push-pos guard "| "))
	      (if last-line (hs-indent-glfsf-push-pos-offset guard 2)))
          ;; "000011"  13= rh arh
          (13 (hs-indent-glfsf-push-pos aft-rhs-sign))
          ;; "000010"  14= rh
          (14 (if last-line (hs-indent-glfsf-push-pos-offset rhs-sign 2 )))
          ;; "000000"  15=
          (t (error "hs-indent-glfsf-empty: %s impossible case" test ))))
      hs-indent-glfsf-info)))

(defun hs-indent-glfsf-ident (start end end-visible indent-info)
  "Find indentation points for a line starting with an identifier."
  (save-excursion
    (let*
        ((hs-indent-glfsf-info indent-info)
         (sep (hs-indent-glfsf-separate-valdef start end))
         (valname (pop sep))
         (valname-string (pop sep))
         (aft-valname (pop sep))
         (guard (pop sep))
         (aft-guard (pop sep))
         (rhs-sign (pop sep))
         (aft-rhs-sign (pop sep))
         (last-line (= end end-visible))
         (is-where
          (string-match "where[ \t]*" hs-indent-glfsf-current-line-first-ident))
         (diff-first                 ; not a function def with the same name
          (not(string= valname-string hs-indent-glfsf-current-line-first-ident)))
         ;; (is-type-def
         ;;  (and rhs-sign (eq (char-after rhs-sign) ?\:)))
         (test (string
                (if valname ?1 ?0)
                (if (and aft-valname (< aft-valname end-visible)) ?1 ?0)
                (if (and guard (< guard end-visible)) ?1 ?0)
                (if (and aft-guard (< aft-guard end-visible)) ?1 ?0)
                (if (and rhs-sign (< rhs-sign end-visible)) ?1 ?0)
                (if (and aft-rhs-sign (< aft-rhs-sign end-visible)) ?1 ?0))))
      (if (and valname-string           ; special case for start keywords
               (string-match hs-indent-glfsf-start-keywords-re valname-string))
          (progn
            (hs-indent-glfsf-push-pos valname)
            (if (string-match "\\<data\\>" valname-string)
                ;; very special for data keyword
                (if aft-rhs-sign (hs-indent-glfsf-push-pos aft-rhs-sign)
                  (hs-indent-glfsf-push-pos-offset valname))
              (if (not (string-match
                        hs-indent-glfsf-start-keywords-re
                        hs-indent-glfsf-current-line-first-ident))
                  (hs-indent-glfsf-push-pos-offset valname))))
        (if (string= hs-indent-glfsf-current-line-first-ident "::")
            (if valname (hs-indent-glfsf-push-pos valname))
          (case                         ; general case
              (hs-indent-glfsf-find-case test)
            ;; "1.1.11"   1= vn gd rh arh
            (1 (if is-where
                   (hs-indent-glfsf-push-pos guard)
                 (hs-indent-glfsf-push-pos valname)
                 (if diff-first (hs-indent-glfsf-push-pos aft-rhs-sign))))
            ;; "1.1.10"   2= vn gd rh
            (2 (if is-where
                   (hs-indent-glfsf-push-pos guard)
                 (hs-indent-glfsf-push-pos valname)
                 (if last-line
                     (hs-indent-glfsf-push-pos-offset guard))))
            ;; "1.1100"   3= vn gd agd
            (3 (if is-where
                   (hs-indent-glfsf-push-pos-offset guard)
                 (hs-indent-glfsf-push-pos valname)
                 (if diff-first
                     (hs-indent-glfsf-push-pos aft-guard))))
            ;; "1.1000"   4= vn gd
            (4 (if is-where
                   (hs-indent-glfsf-push-pos guard)
                 (hs-indent-glfsf-push-pos valname)
                 (if last-line
                     (hs-indent-glfsf-push-pos-offset guard 2))))
            ;; "1.0011"   5= vn rh arh
            (5 (if is-where
                   (hs-indent-glfsf-push-pos-offset valname)
                 (hs-indent-glfsf-push-pos valname)
                 (if diff-first
                     (hs-indent-glfsf-push-pos aft-rhs-sign))))
            ;; "1.0010"   6= vn rh
            (6 (if is-where
                   (hs-indent-glfsf-push-pos-offset valname)
                 (hs-indent-glfsf-push-pos valname)
                 (if last-line
                     (hs-indent-glfsf-push-pos-offset valname))))
            ;; "110000"   7= vn avn
            (7 (if is-where
                   (hs-indent-glfsf-push-pos-offset valname)
                 (hs-indent-glfsf-push-pos valname)
                 (if last-line
                     (hs-indent-glfsf-push-pos aft-valname))))
            ;; "100000"   8= vn
            (8 (if is-where
                   (hs-indent-glfsf-push-pos-offset valname)
                 (hs-indent-glfsf-push-pos valname)))
            ;; "001.11"   9= gd rh arh
            (9 (if is-where
                   (hs-indent-glfsf-push-pos guard)
                 (hs-indent-glfsf-push-pos aft-rhs-sign)))
            ;; "001.10"  10= gd rh
            (10 (if is-where
                    (hs-indent-glfsf-push-pos guard)
                  (if last-line
                      (hs-indent-glfsf-push-pos-offset guard))))
            ;; "001100"  11= gd agd
            (11 (if is-where
                    (hs-indent-glfsf-push-pos guard)
                  (if (hs-indent-glfsf-no-otherwise guard)
                      (hs-indent-glfsf-push-pos aft-guard))))
            ;; "001000"  12= gd
            (12 (if last-line (hs-indent-glfsf-push-pos-offset guard 2)))
            ;; "000011"  13= rh arh
            (13 (hs-indent-glfsf-push-pos aft-rhs-sign))
            ;; "000010"  14= rh
            (14 (if last-line (hs-indent-glfsf-push-pos-offset rhs-sign 2)))
            ;; "000000"  15=
            (t (error "hs-indent-glfsf-ident: %s impossible case" test )))))
      hs-indent-glfsf-info)))

(defun hs-indent-glfsf-other (start end end-visible indent-info)
  "Find indentation points for a non-empty line starting with something other
than an identifier, a guard or rhs."
  (save-excursion
    (let* ((hs-indent-glfsf-info indent-info)
           (sep (hs-indent-glfsf-separate-valdef start end))
           (valname (pop sep))
           (valname-string (pop sep))
           (aft-valname (pop sep))
           (guard (pop sep))
           (aft-guard (pop sep))
           (rhs-sign (pop sep))
           (aft-rhs-sign (pop sep))
           (last-line (= end end-visible))
           (test (string
                  (if valname ?1 ?0)
                  (if (and aft-valname (< aft-valname end-visible)) ?1 ?0)
                  (if (and guard (< guard end-visible)) ?1 ?0)
                  (if (and aft-guard (< aft-guard end-visible)) ?1 ?0)
                  (if (and rhs-sign (< rhs-sign end-visible)) ?1 ?0)
                  (if (and aft-rhs-sign (< aft-rhs-sign end-visible)) ?1 ?0))))
      (if (and valname-string           ; special case for start keywords
               (string-match hs-indent-glfsf-start-keywords-re valname-string))
          (hs-indent-glfsf-push-pos-offset valname)
        (case                           ; general case
         (hs-indent-glfsf-find-case test)
         ;; "1.1.11"   1= vn gd rh arh
         (1 (hs-indent-glfsf-push-pos aft-rhs-sign))
         ;; "1.1.10"   2= vn gd rh
         (2 (if last-line
                   (hs-indent-glfsf-push-pos-offset guard)
               (hs-indent-glfsf-push-pos-offset rhs-sign 2)))
         ;; "1.1100"   3= vn gd agd
         (3 (hs-indent-glfsf-push-pos aft-guard))
         ;; "1.1000"   4= vn gd
         (4 (hs-indent-glfsf-push-pos-offset guard 2))
         ;; "1.0011"   5= vn rh arh
         (5 (hs-indent-glfsf-push-pos valname)
            (hs-indent-glfsf-push-pos aft-rhs-sign))
         ;; "1.0010"   6= vn rh
         (6 (if last-line
                (hs-indent-glfsf-push-pos-offset valname)
              (hs-indent-glfsf-push-pos-offset rhs-sign 2)))
         ;; "110000"   7= vn avn
         (7 (hs-indent-glfsf-push-pos-offset aft-valname))
         ;; "100000"   8= vn
         (8 (hs-indent-glfsf-push-pos valname))
         ;; "001.11"   9= gd rh arh
         (9 (hs-indent-glfsf-push-pos aft-rhs-sign))
         ;; "001.10"  10= gd rh
         (10 (if last-line
                   (hs-indent-glfsf-push-pos-offset guard)
               (hs-indent-glfsf-push-pos-offset rhs-sign 2)))
         ;; "001100"  11= gd agd
         (11 (if (hs-indent-glfsf-no-otherwise guard)
                   (hs-indent-glfsf-push-pos aft-guard)))
         ;; "001000"  12= gd
         (12 (if last-line (hs-indent-glfsf-push-pos-offset guard 2)))
         ;; "000011"  13= rh arh
         (13 (hs-indent-glfsf-push-pos aft-rhs-sign))
         ;; "000010"  14= rh
         (14 (if last-line (hs-indent-glfsf-push-pos-offset rhs-sign 2)))
         ;; "000000"  15=
         (t (error "hs-indent-glfsf-other: %s impossible case" test ))))
      hs-indent-glfsf-info)))

(defun hs-indent-glfsf-valdef-indentation (start end end-visible curr-line-type
                                          indent-info)
  "Find indentation information for a value definition."
  (let ((hs-indent-glfsf-info indent-info))
    (if (< start end-visible)
        (case curr-line-type
          (empty (hs-indent-glfsf-empty start end end-visible indent-info))
          (ident (hs-indent-glfsf-ident start end end-visible indent-info))
          (guard (hs-indent-glfsf-guard start end end-visible indent-info))
          (rhs   (hs-indent-glfsf-rhs start end end-visible indent-info))
          (comment (error "Comment indent should never happen"))
          (other (hs-indent-glfsf-other start end end-visible indent-info)))
      hs-indent-glfsf-info)))

(defun hs-indent-glfsf-line-indentation (line-start line-end end-visible
                                         curr-line-type indent-info)
  "Compute indentation info between LINE-START and END-VISIBLE.
Separate a line of program into valdefs between offside keywords
and find indentation info for each part."
  (save-excursion
    ;; point is (already) at line-start
    (assert (eq (point) line-start))
    (let ((hs-indent-glfsf-info indent-info)
          (start (or (hs-indent-glfsf-in-comment line-start line-end)
                     (hs-indent-glfsf-in-string line-start line-end))))
      (if start                         ; if comment at the end
          (setq line-end start))  ; end line before it
      ;; loop on all parts separated by off-side-keywords
      (while (and (re-search-forward hs-indent-glfsf-off-side-keywords-re
                                     line-end t)
                  (not (or (hs-indent-glfsf-in-comment line-start (point))
                           (hs-indent-glfsf-in-string line-start (point)))))
	(let ((beg-match (match-beginning 0)) ; save beginning of match
	      (end-match (match-end 0)))      ; save end of match
          ;; Do not try to find indentation points if off-side-keyword at
          ;; the start...
          (if (or (< line-start beg-match)
                  ;; Actually, if we're looking at a "let" inside a "do", we
                  ;; should add the corresponding indentation point.
                  (eq (char-after beg-match) ?l))
              (setq hs-indent-glfsf-info
                    (hs-indent-glfsf-valdef-indentation line-start beg-match
                                                       end-visible
                                                       curr-line-type
                                                       hs-indent-glfsf-info)))
          ;; ...but keep the start of the line if keyword alone on the line
          (if (= line-end end-match)
              (hs-indent-glfsf-push-pos beg-match))
          (setq line-start end-match)
          (goto-char line-start)))
      (hs-indent-glfsf-valdef-indentation line-start line-end end-visible
                                         curr-line-type hs-indent-glfsf-info))))


(defun hs-indent-glfsf-layout-indent-info (start contour-line)
  (let ((hs-indent-glfsf-info nil)
        (curr-line-type (hs-indent-glfsf-type-at-point))
	line-start line-end end-visible)
    (save-excursion
      (if (eq curr-line-type 'ident)
	  (let				; guess the type of line
	      ((sep
		(hs-indent-glfsf-separate-valdef
		 (point) (line-end-position))))
	    ;; if the first ident is where or the start of a def
	    ;; keep it in a global variable
	    (setq hs-indent-glfsf-current-line-first-ident
		  (if (string-match "where[ \t]*" (nth 1 sep))
		      (nth 1 sep)
		    (if (nth 5 sep)		; is there a rhs-sign
			(if (= (char-after (nth 5 sep)) ?\:) ;is it a typdef
			    "::" (nth 1 sep))
		      "")))))
      (while contour-line		; explore the contour points
	(setq line-start (pop contour-line))
	(goto-char line-start)
	(setq line-end (line-end-position))
	(setq end-visible		; visible until the column of the
	      (if contour-line		; next contour point
		  (save-excursion
		    (move-to-column
		     (hs-indent-glfsf-point-to-col (car contour-line)))
		    (point))
		line-end))
	(unless (or (hs-indent-glfsf-open-structure start line-start)
		    (hs-indent-glfsf-in-comment start line-start))
	  (setq hs-indent-glfsf-info
		(hs-indent-glfsf-line-indentation line-start line-end
						 end-visible curr-line-type
						 hs-indent-glfsf-info)))))
    hs-indent-glfsf-info))

(defun hs-indent-glfsf-find-matching-start (regexp limit &optional pred start)
  (let ((open (hs-indent-glfsf-open-structure limit (point))))
    (if open (setq limit (1+ open))))
  (unless start (setq start (point)))
  (when (re-search-backward regexp limit t)
    (let ((nestedcase (match-end 1))
          (outer (or (hs-indent-glfsf-in-string limit (point))
                     (hs-indent-glfsf-in-comment limit (point))
                     (hs-indent-glfsf-open-structure limit (point))
                     (if (and pred (funcall pred start)) (point)))))
      (cond
       (outer
        (goto-char outer)
        (hs-indent-glfsf-find-matching-start regexp limit pred start))
       (nestedcase
        ;; Nested case.
        (and (hs-indent-glfsf-find-matching-start regexp limit pred)
             (hs-indent-glfsf-find-matching-start regexp limit pred start)))
       (t (point))))))

(defun hs-indent-glfsf-filter-let-no-in (start)
  "Return non-nil if point is in front of a `let' that has no `in'.
START is the position of the presumed `in'."
  ;; We're looking at either `in' or `let'.
  (when (looking-at "let")
    (ignore-errors
      (save-excursion
        (forward-word 1)
        (forward-comment (point-max))
        (if (looking-at "{")
            (progn
              (forward-sexp 1)
              (forward-comment (point-max))
              (< (point) start))
          ;; Use the layout rule to see whether this let is already closed
          ;; without an `in'.
          (let ((col (current-column)))
            (while (progn (forward-line 1) (hs-indent-glfsf-back-to-indentation)
                          (< (point) start))
              (when (< (current-column) col)
                (setq col nil)
                (goto-char start)))
            (null col)))))))

(defun hs-indent-glfsf-comment (open start)
  "Compute indent info for comments and text inside comments.
OPEN is the start position of the comment in which point is."
  ;; Ideally we'd want to guess whether it's commented out code or
  ;; whether it's text.  Instead, we'll assume it's text.
  (save-excursion
    (if (= open (point))
	;; We're actually just in front of a comment: align with following
	;; code or with comment on previous line.
        (let ((prev-line-info
               (cond
                ((eq (char-after) ?\{) nil) ;Align as if it were code.
                ((and (forward-comment -1)
                      (> (line-beginning-position 3) open))
                 ;; We're after another comment and there's no empty line
                 ;; between us.
                 (list (list (hs-indent-glfsf-point-to-col (point)))))
                (t nil))))              ;Else align as if it were code
          ;; Align with following code.
          (forward-comment (point-max))
          ;; There are several possible indentation points for this code-line,
          ;; but the only valid indentation point for the comment is the one
          ;; that the user will select for the code-line.  Obviously we can't
          ;; know that, so we just assume that the code-line is already at its
          ;; proper place.
          ;; Strictly speaking "assume it's at its proper place" would mean
          ;; we'd just use (current-column), but since this is using info from
          ;; lines further down and it's common to reindent line-by-line,
          ;; we'll align not with the current indentation, but with the
          ;; one that auto-indentation "will" select.
          (append
           prev-line-info
           (let ((indent-info (save-excursion
                                (hs-indent-glfsf-indentation-info start)))
                 (col (current-column)))
             ;; Sort the indent-info so that the current indentation comes
             ;; out first.
             (setq indent-info
                   (sort indent-info
                         (lambda (x y)
                           (<= (abs (- col (car x))) (abs (- col (car y)))))))
             indent-info)))

      ;; We really are inside a comment.
      (if (looking-at "-}")
	  (progn
	    (forward-char 2)
	    (forward-comment -1)
            (list (list (1+ (hs-indent-glfsf-point-to-col (point))))))
	(let ((offset (if (looking-at "--?")
			  (- (match-beginning 0) (match-end 0)))))
	  (forward-line -1)		;Go to previous line.
	  (hs-indent-glfsf-back-to-indentation)
	  (if (< (point) start) (goto-char start))

          (list (list (if (looking-at comment-start-skip)
                          (if offset
                              (+ 2 offset (hs-indent-glfsf-point-to-col (point)))
                            (hs-indent-glfsf-point-to-col (match-end 0)))
                        (hs-indent-glfsf-point-to-col (point))))))))))

(defcustom hs-indent-glfsf-thenelse 0
  "If non-nil, \"then\" and \"else\" are indented.
This is necessary in the \"do\" layout under Haskell-98.
See http://hackage.haskell.org/trac/haskell-prime/wiki/DoAndIfThenElse"
  :group 'hs-indent-glfsf
  :type 'integer)

(defun hs-indent-glfsf-closing-keyword (start)
  (let ((open (save-excursion
                (hs-indent-glfsf-find-matching-start
                 (case (char-after)
                   (?i "\\<\\(?:\\(in\\)\\|let\\)\\>")
                   (?o "\\<\\(?:\\(of\\)\\|case\\)\\>")
                   (?t "\\<\\(?:\\(then\\)\\|if\\)\\>")
                   (?e "\\<\\(?:\\(else\\)\\|if\\)\\>"))
                 start
                 (if (eq (char-after) ?i)
                     ;; Filter out the `let's that have no `in'.
                     'hs-indent-glfsf-filter-let-no-in)))))
    ;; For a "hanging let/case/if at EOL" we should use a different
    ;; indentation scheme.
    (save-excursion
      (goto-char open)
      (if (hs-indent-glfsf-hanging-p)
          (setq open (hs-indent-glfsf-virtual-indentation start))))
    ;; FIXME: we should try and figure out if the `if' is in a `do' layout
    ;; before using hs-indent-glfsf-thenelse.
    (list (list (+ (if (memq (char-after) '(?t ?e)) hs-indent-glfsf-thenelse 0)
                   (hs-indent-glfsf-point-to-col open))))))

(defcustom hs-indent-glfsf-after-keywords
  '(("where" 2 0)
    ("of" 2)
    ("do" 2)
    ("in" 2 0)
    ("{" 2)
    "if"
    "then"
    "else"
    "let")
  "Keywords after which indentation should be indented by some offset.
Each keyword info can have the following forms:

   KEYWORD | (KEYWORD OFFSET [OFFSET-HANGING])

If absent OFFSET-HANGING defaults to OFFSET.
If absent OFFSET defaults to `hs-indent-glfsf-offset'.

OFFSET-HANGING is the offset to use in the case where the keyword
is at the end of an otherwise-non-empty line."
  :group 'hs-indent-glfsf
  :type '(repeat (choice string
                         (cons :tag "" (string :tag "keyword:")
                         (cons :tag "" (integer :tag "offset")
                         (choice (const nil)
                                 (list :tag ""
                                       (integer :tag "offset-pending"))))))))

(defun hs-indent-glfsf-skip-lexeme-forward ()
  (and (zerop (skip-syntax-forward "w"))
       (skip-syntax-forward "_")
       (skip-syntax-forward "(")
       (skip-syntax-forward ")")))

(defvar hs-indent-glfsf-inhibit-after-offset nil)

(defun hs-indent-glfsf-offset-after-info ()
  "Return the info from `hs-indent-glfsf-after-keywords' for keyword at point."
  (let ((id (buffer-substring
             (point)
             (save-excursion
               (hs-indent-glfsf-skip-lexeme-forward)
               (point)))))
    (or (assoc id hs-indent-glfsf-after-keywords)
        (car (member id hs-indent-glfsf-after-keywords)))))

(defcustom hs-indent-glfsf-dont-hang '("(")
  "Lexemes that should never be considered as hanging."
  :group 'hs-indent-glfsf
  :type '(repeat string))

(defun hs-indent-glfsf-hanging-p ()
  ;; A Hanging keyword is one that's at the end of a line except it's not at
  ;; the beginning of a line.
  (not (or (= (current-column) (hs-indent-glfsf-current-indentation))
           (save-excursion
             (let ((lexeme
                    (buffer-substring
                     (point)
                     (progn (hs-indent-glfsf-skip-lexeme-forward) (point)))))
               (or (member lexeme hs-indent-glfsf-dont-hang)
                   (> (line-end-position)
                      (progn (forward-comment (point-max)) (point)))))))))

(defun hs-indent-glfsf-after-keyword-column (offset-info start &optional default)
  (unless offset-info
    (setq offset-info (hs-indent-glfsf-offset-after-info)))
  (unless default (setq default hs-indent-glfsf-offset))
  (setq offset-info
        (if hs-indent-glfsf-inhibit-after-offset '(0) (cdr-safe offset-info)))
  (if (not (hs-indent-glfsf-hanging-p))
      (hs-indent-glfsf-column+offset (current-column)
                                    (or (car offset-info) default))
    ;; The keyword is hanging at the end of the line.
    (hs-indent-glfsf-column+offset
     (hs-indent-glfsf-virtual-indentation start)
     (or (cadr offset-info) (car offset-info) default))))

(defun hs-indent-glfsf-inside-paren (open)
  ;; there is an open structure to complete
  (if (looking-at "\\s)\\|[;,]")
      ;; A close-paren or a , or ; can only correspond syntactically to
      ;; the open-paren at `open'.  So there is no ambiguity.
      (progn
        (if (or (and (eq (char-after) ?\;) (eq (char-after open) ?\())
                (and (eq (char-after) ?\,) (eq (char-after open) ?\{)))
            (message "Mismatched punctuation: `%c' in %c...%c"
                     (char-after) (char-after open)
                     (if (eq (char-after open) ?\() ?\) ?\})))
        (save-excursion
          (goto-char open)
          (list (list
                 (if (hs-indent-glfsf-hanging-p)
                     (hs-indent-glfsf-virtual-indentation nil)
                   (hs-indent-glfsf-point-to-col open))))))
    ;; There might still be layout within the open structure.
    (let* ((end (point))
           (basic-indent-info
             ;; Anything else than a ) is subject to layout.
             (if (looking-at "\\s.\\|\\$ ")
                 (hs-indent-glfsf-point-to-col open) ; align a punct with (
               (let ((follow (save-excursion
                               (goto-char (1+ open))
                               (hs-indent-glfsf-skip-blanks-and-newlines-forward end)
                               (point))))
                 (if (= follow end)
                     (save-excursion
                       (goto-char open)
                       (hs-indent-glfsf-after-keyword-column nil nil 1))
                   (hs-indent-glfsf-point-to-col follow)))))
           (open-column (hs-indent-glfsf-point-to-col open))
           (contour-line (hs-indent-glfsf-contour-line (1+ open) end)))
      (if (null contour-line)
          (list (list basic-indent-info))
        (let ((indent-info
               (hs-indent-glfsf-layout-indent-info
                (1+ open) contour-line)))
          ;; Fix up indent info.
          (let ((base-elem (assoc open-column indent-info)))
            (if base-elem
                (progn (setcar base-elem basic-indent-info)
                       (setcdr base-elem nil))
              (setq indent-info
                    (append indent-info (list (list basic-indent-info)))))
            indent-info))))))

(defun hs-indent-glfsf-virtual-indentation (start)
  "Compute the \"virtual indentation\" of text at point.
The \"virtual indentation\" is the indentation that text at point would have
had, if it had been placed on its own line."
  (let ((col (current-column))
        (hs-indent-glfsf-inhibit-after-offset (hs-indent-glfsf-hanging-p)))
    (if (save-excursion (skip-chars-backward " \t") (bolp))
        ;; If the text is indeed on its own line, than the virtual indent is
        ;; the current indentation.
        col
      ;; Else, compute the indentation that it would have had.
      (let ((info (hs-indent-glfsf-indentation-info start))
            (max -1))
        ;; `info' is a list of possible indent points.  Each indent point is
        ;; assumed to correspond to a different parse.  So we need to find
        ;; the parse that corresponds to the case at hand (where there's no
        ;; line break), which is assumed to always be the
        ;; deepest indentation.
        (dolist (x info)
          (setq x (car x))
          ;; Sometimes `info' includes the current indentation (or yet
          ;; deeper) by mistake, because hs-indent-glfsf-indentation-info
          ;; wasn't designed to be called on a piece of text that is not at
          ;; BOL.  So ignore points past `col'.
          (if (and (> x max) (not (>= x col)))
              (setq max x)))
        ;; In case all the indent points are past `col', just use `col'.
        (if (>= max 0) max col)))))

(defun hs-indent-glfsf-indentation-info (&optional start)
  "Return a list of possible indentations for the current line.
These are then used by `hs-indent-glfsf-cycle'.
START if non-nil is a presumed start pos of the current definition."
  (unless start (setq start (hs-indent-glfsf-start-of-def)))
  (let (open contour-line)
    (cond
     ;; in string?
     ((setq open (hs-indent-glfsf-in-string start (point)))
      (list (list (+ (hs-indent-glfsf-point-to-col open)
                     (if (looking-at "\\\\") 0 1)))))

     ;; in comment ?
     ((setq open (hs-indent-glfsf-in-comment start (point)))
      (hs-indent-glfsf-comment open start))

     ;; Closing the declaration part of a `let' or the test exp part of a case.
     ((looking-at "\\(?:in\\|of\\|then\\|else\\)\\>")
      (hs-indent-glfsf-closing-keyword start))

     ;; Right after a special keyword.
     ((save-excursion
        (forward-comment (- (point-max)))
        (when (and (not (zerop (skip-syntax-backward "w")))
                   (setq open (hs-indent-glfsf-offset-after-info)))
          (list (list (hs-indent-glfsf-after-keyword-column open start))))))

     ;; open structure? ie  ( { [
     ((setq open (hs-indent-glfsf-open-structure start (point)))
      (hs-indent-glfsf-inside-paren open))

     ;; full indentation
     ((setq contour-line (hs-indent-glfsf-contour-line start (point)))
      (hs-indent-glfsf-layout-indent-info start contour-line))

     (t
      ;; simple contour just one indentation at start
      (list (list (if (and (eq hs-mode-literate 'bird)
                           (eq (hs-indent-glfsf-point-to-col start) 1))
                      ;; for a Bird style literate script put default offset
                      ;; in the case of no indentation
                      (1+ hs-indent-glfsf-literate-Bird-default-offset)
                    (hs-indent-glfsf-point-to-col start))))))))

(defvar hs-indent-glfsf-last-info nil)


(defun hs-indent-glfsf-cycle ()
  "Indentation cycle.
We stay in the cycle as long as the TAB key is pressed."
  (interactive "*")
  (if (and hs-mode-literate
           (not (hs-indent-glfsf-within-literate-code)))
      ;; use the ordinary tab for text...
      (funcall (default-value 'indent-line-function))
    (let ((marker (if (> (current-column) (hs-indent-glfsf-current-indentation))
		      (point-marker)))
	  (bol (progn (beginning-of-line) (point))))
      (hs-indent-glfsf-back-to-indentation)
      (unless (and (eq last-command this-command)
		   (eq bol (car hs-indent-glfsf-last-info)))
	(save-excursion
	  (setq hs-indent-glfsf-last-info
		(list bol (hs-indent-glfsf-indentation-info) 0 0))))

      (let* ((il (nth 1 hs-indent-glfsf-last-info))
	     (index (nth 2 hs-indent-glfsf-last-info))
	     (last-insert-length (nth 3 hs-indent-glfsf-last-info))
	     (indent-info (nth index il)))

	(hs-indent-glfsf-line-to (car indent-info)) ; insert indentation
	(delete-char last-insert-length)
	(setq last-insert-length 0)
	(let ((text (cdr indent-info)))
	  (if text
	      (progn
		(insert text)
		(setq last-insert-length (length text)))))

	(setq hs-indent-glfsf-last-info
	      (list bol il (% (1+ index) (length il)) last-insert-length))

	(if (= (length il) 1)
	    (message "Sole indentation")
	  (message "Indent cycle (%d)..." (length il)))

	(if marker
	    (goto-char (marker-position marker)))))))

(defun hs-indent-glfsf-region (start end)
  (error "Auto-reindentation of a region is not supported"))

;;; alignment functions

(defun hs-indent-glfsf-shift-columns (dest-column region-stack)
  "Shift columns in REGION-STACK to go to DEST-COLUMN.
Elements of the stack are pairs of points giving the start and end
of the regions to move."
  (let (reg col diffcol reg-end)
    (while (setq reg (pop region-stack))
      (setq reg-end (copy-marker (cdr reg)))
      (goto-char (car reg))
      (setq col (current-column))
      (setq diffcol (- dest-column col))
      (if (not (zerop diffcol))
          (catch 'end-of-buffer
            (while (<= (point) (marker-position reg-end))
              (if (< diffcol 0)
                  (backward-delete-char-untabify (- diffcol) nil)
                (insert-char ?\  diffcol))
              (end-of-line 2)           ; should be (forward-line 1)
              (if (eobp)                ; but it adds line at the end...
                  (throw 'end-of-buffer nil))
              (move-to-column col)))))))

(defun hs-indent-glfsf-align-def (p-arg type)
  "Align guards or rhs within the current definition before point.
If P-ARG is t align all defs up to the mark.
TYPE is either 'guard or 'rhs."
  (save-excursion
    (let (start-block end-block
          (maxcol (if (eq type 'rhs) hs-indent-glfsf-rhs-align-column 0))
          contour sep defname defnamepos
          defcol pos lastpos
          regstack eqns-start start-found)
      ;; find the starting and ending boundary points for alignment
      (if p-arg
          (if (mark)                    ; aligning everything in the region
            (progn
              (when (> (mark) (point)) (exchange-point-and-mark))
              (setq start-block
                    (save-excursion
                      (goto-char (mark))
                      (line-beginning-position)))
              (setq end-block
                  (progn (if (hs-indent-glfsf-bolp)
                             (hs-indent-glfsf-forward-line -1))
                         (line-end-position))))
            (error "The mark is not set for aligning definitions"))
        ;; aligning the current definition
        (setq start-block (hs-indent-glfsf-start-of-def))
        (setq end-block (line-end-position)))
      ;; find the start of the current valdef using the contour line
      ;; in reverse order because we need the nearest one from the end
      (setq contour
            (reverse (hs-indent-glfsf-contour-line start-block end-block)))
      (setq pos (car contour))          ; keep the start of the first contour
      ;; find the nearest start of a definition
      (while (and (not defname) contour)
        (goto-char (pop contour))
        (if (hs-indent-glfsf-open-structure start-block (point))
            nil
          (setq sep (hs-indent-glfsf-separate-valdef (point) end-block))
          (if (nth 5 sep)               ; is there a rhs?
              (progn (setq defnamepos (nth 0 sep))
                     (setq defname (nth 1 sep))))))
      ;; start building the region stack
      (if defnamepos
          (progn                        ; there is a valdef
            ;; find the start of each equation or guard
            (if p-arg      ; when indenting a region
                ;; accept any start of id or pattern as def name
                (setq defname "\\<\\|("))
            (setq defcol (hs-indent-glfsf-point-to-col defnamepos))
            (goto-char pos)
            (setq end-block (line-end-position))
            (catch 'top-of-buffer
              (while (and (not start-found)
                          (>= (point) start-block))
                (if (<= (hs-indent-glfsf-current-indentation) defcol)
                    (progn
                      (move-to-column defcol)
                      (if (and (looking-at defname) ; start of equation
                               (not (hs-indent-glfsf-open-structure start-block (point))))
                          (push (cons (point) 'eqn) eqns-start)
                        ;; found a less indented point not starting an equation
                        (setq start-found t)))
                  ;; more indented line
                  (hs-indent-glfsf-back-to-indentation)
                  (if (and (eq (hs-indent-glfsf-type-at-point) 'guard) ; start of a guard
                           (not (hs-indent-glfsf-open-structure start-block (point))))
                      (push (cons (point) 'gd) eqns-start)))
                (if (bobp)
                    (throw 'top-of-buffer nil)
                  (hs-indent-glfsf-backward-to-indentation 1))))
            ;; remove the spurious guards before the first equation
            (while (and eqns-start (eq (cdar eqns-start) 'gd))
              (pop eqns-start))
            ;; go through each equation to find the region to indent
            (while eqns-start
              (let ((eqn (caar eqns-start)))
		(setq lastpos (if (cdr eqns-start)
				  (save-excursion
				    (goto-char (caadr eqns-start))
				    (hs-indent-glfsf-forward-line -1)
				    (line-end-position))
				end-block))
		(setq sep (hs-indent-glfsf-separate-valdef eqn lastpos)))
              (if (eq type 'guard)
                  (setq pos (nth 3 sep))
                ;; check if what follows a rhs sign is more indented or not
                (let ((rhs (nth 5 sep))
                      (aft-rhs (nth 6 sep)))
                  (if (and rhs aft-rhs
                           (> (hs-indent-glfsf-point-to-col rhs)
                              (hs-indent-glfsf-point-to-col aft-rhs)))
                      (setq pos aft-rhs)
                    (setq pos rhs))))
              (if pos
                  (progn                ; update region stack
                    (push (cons pos (or lastpos pos)) regstack)
                    (setq maxcol        ; find the highest column number
                          (max maxcol
                               (progn   ;find the previous non-empty column
                                 (goto-char pos)
                                 (skip-chars-backward
                                  " \t"
                                  (line-beginning-position))
                                 (if (hs-indent-glfsf-bolp)
                                     ;;if on an empty prefix
                                     (hs-indent-glfsf-point-to-col pos) ;keep original indent
                                   (1+ (hs-indent-glfsf-point-to-col (point)))))))))
              (pop eqns-start))
            ;; now shift according to the region stack
            (if regstack
                (hs-indent-glfsf-shift-columns maxcol regstack)))))))

(defun hs-indent-glfsf-align-guards-and-rhs (start end)
  "Align the guards and rhs of functions in the region, which must be active."
  ;; The `start' and `end' args are dummys right now: they're just there so
  ;; we can use the "r" interactive spec which properly signals an error.
  (interactive "*r")
  (hs-indent-glfsf-align-def t 'guard)
  (hs-indent-glfsf-align-def t 'rhs))

;;;  insertion functions

(defun hs-indent-glfsf-insert-equal ()
  "Insert an = sign and align the previous rhs of the current function."
  (interactive "*")
  (if (or (hs-indent-glfsf-bolp)
          (/= (preceding-char) ?\ ))
      (insert ?\ ))
  (insert "= ")
  (hs-indent-glfsf-align-def (hs-indent-glfsf-mark-active) 'rhs))

(defun hs-indent-glfsf-insert-guard (&optional text)
  "Insert and align a guard sign (|) followed by optional TEXT.
Alignment works only if all guards are to the south-east of their |."
  (interactive "*")
  (let ((pc (if (hs-indent-glfsf-bolp) ?\012
                (preceding-char)))
        (pc1 (or (char-after (- (point) 2)) 0)))
    ;; check what guard to insert depending on the previous context
    (if (= pc ?\ )                      ; x = any char other than blank or |
        (if (/= pc1 ?\|)
            (insert "| ")               ; after " x"
          ())                           ; after " |"
      (if (= pc ?\|)
          (if (= pc1 ?\|)
              (insert " | ")            ; after "||"
            (insert " "))               ; after "x|"
        (insert " | ")))                ; general case
    (if text (insert text))
    (hs-indent-glfsf-align-def (hs-indent-glfsf-mark-active) 'guard)))

(defun hs-indent-glfsf-insert-otherwise ()
  "Insert a guard sign (|) followed by `otherwise'.
Also align the previous guards of the current function."
  (interactive "*")
  (hs-indent-glfsf-insert-guard "otherwise")
  (hs-indent-glfsf-insert-equal))

(defun hs-indent-glfsf-insert-where ()
  "Insert a where keyword at point and indent resulting line.
One indentation cycle is used."
  (interactive "*")
  (insert "where ")
  (hs-indent-glfsf-cycle))


;;; hs-indent-glfsf-mode

(defvar hs-indent-glfsf-mode nil
  "Non-nil if the semi-intelligent Haskell indentation mode is in effect.")
(make-variable-buffer-local 'hs-indent-glfsf-mode)

(defvar hs-indent-glfsf-map
  (let ((map (make-sparse-keymap)))
    ;; Removed: remapping DEL seems a bit naughty --SDM
    ;; (define-key map "\177"  'backward-delete-char-untabify)
    ;; The binding to TAB is already handled by indent-line-function.  --Stef
    ;; (define-key map "\t"    'hs-indent-glfsf-cycle)
    (define-key map [?\C-c ?\C-=] 'hs-indent-glfsf-insert-equal)
    (define-key map [?\C-c ?\C-|] 'hs-indent-glfsf-insert-guard)
    ;; Alternate binding, in case C-c C-| is too inconvenient to type.
    ;; Duh, C-g is a special key, let's not use it here.
    ;; (define-key map [?\C-c ?\C-g] 'hs-indent-glfsf-insert-guard)
    (define-key map [?\C-c ?\C-o] 'hs-indent-glfsf-insert-otherwise)
    (define-key map [?\C-c ?\C-w] 'hs-indent-glfsf-insert-where)
    (define-key map [?\C-c ?\C-.] 'hs-indent-glfsf-align-guards-and-rhs)
    (define-key map [?\C-c ?\C->] 'hs-indent-glfsf-put-region-in-literate)
    map))

(defun turn-on-hs-indent-glfsf ()
  "Turn on ``intelligent'' Haskell indentation mode."
  (set (make-local-variable 'indent-line-function) 'hs-indent-glfsf-cycle)
  (set (make-local-variable 'indent-region-function) 'hs-indent-glfsf-region)
  (setq hs-indent-glfsf-mode t)
  ;; Activate our keymap.
  (let ((map (current-local-map)))
    (while (and map (not (eq map hs-indent-glfsf-map)))
      (setq map (keymap-parent map)))
    (if map
        ;; hs-indent-glfsf-map is already active: nothing to do.
        nil
      ;; Put our keymap on top of the others.  We could also put it in
      ;; second place, or in a minor-mode.  The minor-mode approach would be
      ;; easier, but it's harder for the user to override it.  This approach
      ;; is the closest in behavior compared to the previous code that just
      ;; used a bunch of local-set-key.
      (set-keymap-parent hs-indent-glfsf-map (current-local-map))
      ;; Protect our keymap.
      (setq map (make-sparse-keymap))
      (set-keymap-parent map hs-indent-glfsf-map)
      (use-local-map map)))
  (run-hooks 'hs-indent-glfsf-hook))

(defun turn-off-hs-indent-glfsf ()
  "Turn off ``intelligent'' Haskell indentation mode."
  (kill-local-variable 'indent-line-function)
  ;; Remove hs-indent-glfsf-map from the local map.
  (let ((map (current-local-map)))
    (while map
      (let ((parent (keymap-parent map)))
        (if (eq hs-indent-glfsf-map parent)
            (set-keymap-parent map (keymap-parent parent))
          (setq map parent)))))
  (setq hs-indent-glfsf-mode nil))

;; Put this minor mode on the global minor-mode-alist.
(or (assq 'hs-indent-glfsf-mode (default-value 'minor-mode-alist))
    (setq-default minor-mode-alist
                  (append (default-value 'minor-mode-alist)
                          '((hs-indent-glfsf-mode " Ind")))))

;;;###autoload
(defun hs-indent-glfsf-mode (&optional arg)
  "``Intelligent'' Haskell indentation mode.
This deals with the layout rule of Haskell.
\\[hs-indent-glfsf-cycle] starts the cycle which proposes new
possibilities as long as the TAB key is pressed.  Any other key
or mouse click terminates the cycle and is interpreted except for
RET which merely exits the cycle.
Other special keys are:
    \\[hs-indent-glfsf-insert-equal]
      inserts an =
    \\[hs-indent-glfsf-insert-guard]
      inserts an |
    \\[hs-indent-glfsf-insert-otherwise]
      inserts an | otherwise =
these functions also align the guards and rhs of the current definition
    \\[hs-indent-glfsf-insert-where]
      inserts a where keyword
    \\[hs-indent-glfsf-align-guards-and-rhs]
      aligns the guards and rhs of the region
    \\[hs-indent-glfsf-put-region-in-literate]
      makes the region a piece of literate code in a literate script

Invokes `hs-indent-glfsf-hook' if not nil."
  (interactive "P")
  (setq hs-indent-glfsf-mode
        (if (null arg) (not hs-indent-glfsf-mode)
          (> (prefix-numeric-value arg) 0)))
  (if hs-indent-glfsf-mode
      (turn-on-hs-indent-glfsf)
    (turn-off-hs-indent-glfsf)))

(provide 'hs-indent-glfsf)

;; arch-tag: e4e5e90a-12e2-4002-b5cb-7b2375710013
;;; hs-indent-glfsf.el ends here
