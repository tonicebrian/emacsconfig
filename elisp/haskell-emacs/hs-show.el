;;; hs-show.el — A parser and pretty printer for Haskell Show values.

;; Copyright (C) 2011 Chris Done

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; It doesn't support some number literals (probably). I'm not
;; precisely sure what values Show will always produce. There is
;; however a test suite available, so patches for extra Show support
;; is welcome and should be easy to test.

;;; Code:

(require 'peg)

(defun hs-show-replace (start end)
  "Replace the given region containing a Show value with a pretty
  printed collapsible version."
  (let ((text (buffer-substring-no-properties start end)))
    (goto-char start)
    (delete-region start end)
    (hs-show-parse-and-insert text)))

(defun hs-show-parse-or-nil (given)
  (condition-case nil
      (let ((max-lisp-eval-depth 2000))
        (car (eval `(peg-parse-string ,hs-show-parser given))))
    (error nil)))

(defun hs-show-parse-and-insert (given)
  "Parse a `string' containing a Show instance value and insert
  it pretty printed into the current buffer."
  (let ((current-column (- (point)
                           (line-beginning-position)))
        (result (car (eval `(peg-parse-string ,hs-show-parser given)))))
    (hs-show-insert-pretty current-column result)))

(defun hs-show-insert-pretty (column tree &optional parens)
  "Insert a Show `tree' into the current buffer with collapsible nodes."
  (case (car tree)
    ('list (let ((start (point)))
             (insert "[")
             (hs-show-mapcar/i (lambda (x i len)
                                 (hs-show-insert-pretty (+ column 1) x)
                                 (unless (> i (- len 2))
                                   (if (< (+ column (length (hs-show-pretty tree parens)))
                                          80)
                                       (insert ",")
                                     (insert (concat ",\n" (hs-show-indent (+ 1 column) ""))))))
                               (cdr tree))
             (insert "]")))
    ('tuple (let ((start (point)))
              (insert "(")
              (hs-show-mapcar/i (lambda (x i len)
                                  (hs-show-insert-pretty (+ column 1) x)
                                  (unless (> i (- len 2))
                                    (if (< (+ column (length (hs-show-pretty tree parens)))
                                           80)
                                        (insert ",")
                                      (insert (concat ",\n" (hs-show-indent (+ 1 column) ""))))))
                                (cdr tree))
              (insert ")")))
    ('record
     (let ((record (cdr tree)) (overlay (list 'nil)))
       (insert (if parens "(" ""))
       (let ((link-start (point)))
         (insert (car record))
         (let ((button (make-text-button link-start (point) :type 'hs-show-toggle-button)))
           (put-text-property link-start (point) 'face 'font-lock-type-face)
           (button-put button 'overlay overlay)))
       (insert " {\n")
       (let ((curly-start (1- (point)))
             (show-len (+ column (length (hs-show-pretty tree parens)))))
         (hs-show-mapcar/i (lambda (field i len)
                             (insert 
                              (hs-show-indent
                               (if (and (> i 0) (< show-len 80)) 0 column)
                               (car field)))
                             (insert " = ")
                             (put-text-property (- (point) 3) (point) 'face
                                                'font-lock-constant-face)
                             (hs-show-insert-pretty
                              (if (< show-len 80)
                                  0
                                (+ (length (car field)) column 3))
                              (cdr field))
                             (unless (> i (- len 2))
                               (if (< show-len 80)
                                   (insert ", ")
                                 (insert ",\n"))))
                           (cdr record))
         (insert (concat "\n" (hs-show-indent column "}")))
         (progn
           (setf (car overlay) (make-overlay curly-start (- (point) 1) nil t))
           (overlay-put (car overlay) 'invisible t))
         (insert (if parens ")" "")))))
    ('num (let ((num-start (point)))
            (insert (format "%d" (cdr tree)))
            (put-text-property num-start (point) 'face 'font-lock-constant-face)))
    ('string (let ((str-start (point)))
               (insert "\"")
               (if (< (+ column (length (cdr tree))) 60)
                   (progn
                     (insert (format "%s" (cdr tree)))
                     (put-text-property (+ 1 str-start) (point) 'face 'font-lock-string-face))
                 (progn
                   (insert "…")
                   (insert (format "%s" (cdr tree)))
                   (let ((overlay (make-overlay (+ 2 str-start) (point) nil t)))
                     (overlay-put overlay 'invisible t)
                     (put-text-property (+ 2 str-start) (point) 'face 'font-lock-string-face)
                     (let ((button (make-text-button (+ 1 str-start) (+ 2 str-start) 
                                                     :type 'hs-show-toggle-button)))
                       (put-text-property (+ 1 str-start) (+ 2 str-start) 
                                          'face 'font-lock-keyword-face)
                       (button-put button 'overlay (list overlay))
                       (button-put button 'hide-on-click t)))))
               (insert "\"")))
    ('data (let ((data (cdr tree)))
             (insert (if parens "(" ""))
             (let ((cons-start (point)))
               (insert (car data))
               (put-text-property cons-start (point) 'face 'font-lock-type-face))
             (unless (null (cdr data))
               (progn (insert " ")
                      (hs-show-mapcar/i
                       (lambda (x i len)
                         (hs-show-insert-pretty column x t)
                         (unless (> i (- len 2))
                           (insert " ")))
                       (cdr data))))
             (insert (if parens ")" ""))))
    ('char (progn (insert "'")
                  (insert (char-to-string (cdr tree)))
                  (put-text-property (- (point) 1) (point) 'face 'font-lock-string-face)
                  (insert "'")))
    ('arbitrary (let ((start (point)))
                  (insert (cdr tree))
                  (put-text-property start (point) 'face 'font-lock-comment-face)))
    (otherwise (error "Unsupported node type: %S" tree))))

(define-button-type 'hs-show-toggle-button
  'action 'hs-show-toggle-button-callback
  'follow-link t
  'help-echo "Click to expand…")

(defun hs-show-toggle-button-callback (btn)
  "The callback to toggle the overlay visibility."
  (let ((overlay (button-get btn 'overlay)))
    (when overlay
      (overlay-put (car overlay) 
                   'invisible (not (overlay-get (car overlay)
                                                'invisible)))))
  (let ((hide (button-get btn 'remove-on-click)))
    (when hide
      (button-put btn 'invisible t))))

(defun hs-show-pretty (tree &optional parens)
  "Show a Show `tree'."
  (case (car tree)
    ('list (format "[%s]"
                   (mapconcat
                    (lambda (x)
                      (hs-show-pretty x))
                    (cdr tree)
                    ",")))
    ('record (let ((record (cdr tree)))
               (format "%s%s {%s}%s"
                       (if parens "(" "")
                       (car record)
                       (mapconcat (lambda (field)
                                    (format "%s = %s"
                                            (car field)
                                            (hs-show-pretty (cdr field))))
                                  (cdr record)
                                  ", ")
                       (if parens ")" ""))))
    ('num (format "%s" (cdr tree)))
    ('string (format "%S" (cdr tree)))
    ('data (let ((data (cdr tree)))
             (format "%s%s%s%s"
                     (if parens "(" "")
                     (car data)
                     (if (null (cdr data))
                         ""
                       (concat " "
                               (mapconcat
                                (lambda (x) (hs-show-pretty x t))
                                (cdr data)
                                " ")))
                     (if parens ")" ""))))
    ('tuple (format "(%s)"
                    (mapconcat
                     (lambda (x)
                       (hs-show-pretty x))
                     (cdr tree)
                     ",")))
    ('char (format "'%s'" (if (= (cdr tree) ?')
                              "\\'"
                            (char-to-string (cdr tree)))))
    ('arbitrary (cdr tree))
    (otherwise (error "Unsupported node type: %S" tree))))

(defun hs-show-test-parser ()
  "Test the parser."
  (interactive)
  (let ((results
         (remove-if
          (lambda (x) (eq x t))
          (mapcar (lambda (test)
                    (let* ((given (car test))
                           (expected (cadr test))
                           (result (car (eval `(peg-parse-string ,hs-show-parser given)))))
                      (if (equal result expected)
                          (if (string= given (hs-show-pretty result))
                              t
                            (if (string-match "^(.*)$" given)
                                t
                              (format "FOR\n\n%s\n\nPRETTY PRINT DIFFERS\n\n%s"
                                      given (hs-show-pretty result))))
                        (format "FOR\n\n%s\n\nEXPECTED\n\n%S\n\nBUT GOT\n\n%S"
                                given expected result))))
                  hs-show-tests))))
    (if (> (length results) 0)
        (message (car results))
      (message (format "OK, passed %s tests."
                       (length hs-show-tests))))))

(defun hs-show-mapcar/i (f xs)
  "Map `f' across `xs' giving the index and length to `f' as extra parameters."
  (let ((len (length xs))
        (i 0))
    (mapcar (lambda (x)
              (funcall f x i len)
              (setq i (1+ i)))
            xs)))

(defun hs-show-indent (n s)
  "Indent a string `s' at colum `n'."
  (concat (make-string n ? )
          s))

(defvar hs-show-parser
  '((show (or parens constructor)) 
    (parens
     (and "(" constructor ")")
     `(inner -- inner))
    (constructor (or number
                     record
                     data
                     string
                     char
                     list-literal
                     tuple-literal))
    (data (list constructor-name
                (* (list " "
                         (or number
                             data
                             string
                             char
                             list-literal
                             tuple-literal
                             parens))))
          `(stuff -- `(data . (,(car stuff) .
                               ,(cadr stuff)))))
    (number (substring (+ [0-9 ?.]))
            `(n -- `(num . ,(string-to-number n))))
    (constructor-name (or (substring ":" (+ (not " ") (any)))
                          (substring [A-Z] (* [A-Z a-z 0-9 ?_ ?']))))
    (string (substring "\"" 
                       (* (or "\\\""
                              (and (not "\"") (any))))
                       "\"")
            `(s -- `(string . ,(read s))))
    (char (substring "'" 
                     (or (and "\\" "'")
                         (and (not "'") (any)))
                     "'")
          `(s -- `(char . ,(string-to-char (read (concat "\"" (substring s 1 -1) "\""))))))
    (list-literal
     (list (substring "[")
           (or (and show
                    (* (and "," show)))
               `(-- nil))
           (substring "]"))
     `(items --
             `(list . ,(remove-if-not #'identity (butlast (cdr items))))))
    (tuple-literal
     (list (substring "(")
           (or (and show
                    (+ (and "," show)))
               `(-- nil))
           (substring ")"))
     `(items --
             `(tuple . ,(remove-if-not #'identity (butlast (cdr items))))))
    (record
     (list constructor-name
           (substring " {")
           record-field
           (* ", " record-field)
           (substring "}"))
     `(parts --
             `(record . ,(cons
                          (car parts)
                          (let ((fields (butlast (cddr parts))))
                            fields)))))
    (record-field
     (list field-name
           (substring " = ")
           show)
     `(parts --
             `(,(car parts) . ,(caddr parts))))
    (field-name (substring [a-z] (* [A-Z a-z 0-9 ?_ ?']))))
  "A parser for show instances.")

(defvar hs-show-tests
  `(("(A)" (data . ("A" . ())))
    ("A" (data . ("A" . ())))
    ("Bar" (data . ("Bar" . ())))
    ("Bar (Mu)" (data . ("Bar" . ((data . ("Mu" . ()))))))
    ("1" (num . 1))
    ("/+0-'24" (arbitrary . "/+0-'24"))
    ("'a'" (char . ?a))
    ("'\\''" (char . ?'))
    (":+" (data . (":+" . ())))
    ("1.1" (num . 1.1))
    ("\"string\""  (string . "string"))
    ("\"st\\\"ring\"" (string . "st\"ring"))
    ("\"str\\265ing\""  (string . "str\265ing"))
    ("[6,9]" (list . ((num . 6) (num . 9))))
    ("[6,[A,B]]" (list . ((num . 6) (list . ((data . ("A" . ())) (data . ("B" . ())))))))
    ("[A,B]" (list . ((data . ("A" . ())) (data . ("B" . ())))))
    ("(1,2)" (tuple . ((num . 1) (num . 2))))
    ("[(1,2),(3,4)]" (list . ((tuple . ((num . 1) (num . 2)))
                              (tuple . ((num . 3) (num . 4))))))
    ("Foo {fooX = 1}" (record . ("Foo" . (("fooX" . (num . 1))))))
    ("Foo {fooX = 1, fooY_ = A, fooZ1 = Foo, fooA' = [1,2], fooB = (1,2), fooC = [A,(B,\"C\")]}"
     (record . ("Foo" .
                (("fooX" . (num . 1))
                 ("fooY_" . (data . ("A" . ())))
                 ("fooZ1" . (data . ("Foo" . ())))
                 ("fooA'" . (list . ((num . 1) (num . 2))))
                 ("fooB" . (tuple . ((num . 1) (num . 2))))
                 ("fooC" . (list . ((data . ("A" . ()))
                                    (tuple . ((data . ("B" . ())) (string . "C")))))))))))
  "Tests for the Show instance parser.")

(provide 'hs-show)