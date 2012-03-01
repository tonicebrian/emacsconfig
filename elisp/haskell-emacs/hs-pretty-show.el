;; A prototype of a firebug/chrome object-inspector.
;;
;; 2011-12-09: Looks good to me, ready to be refactored and packaged
;; up into a module and then added to haskell-emacs REPL.

(require 'peg)

(defun mapcar/i (f xs)
  (let ((len (length xs))
        (i 0))
    (mapcar (lambda (x)
              (funcall f x i len)
              (setq i (1+ i)))
            xs)))

(defun indent (n s)
  (concat (make-string n ? )
          s))

(let* ((hs-show-parser
        '((show constructor)
          (parens
           (and "(" show ")")
           `(inner -- inner))
          (constructor (or number
                           record
                           data
                           string
                           list-literal
                           tuple-literal))
          (data (list constructor-name
                      (* (list " "
                               (or number
                                   data
                                   string
                                   list-literal
                                   tuple-literal
                                   parens))))
                `(stuff -- `(data . (,(car stuff) .
                                     ,(cadr stuff)))))
          (number (substring (+ [0-9 ?.]))
                  `(n -- `(num . ,(string-to-number n))))
          (constructor-name (substring [A-Z] (* [A-Z a-z 0-9 ?_ ?'])))
          (string (substring "\"" 
                             (* (or "\\\""
                                    (and (not "\"") (any))))
                             "\"")
                  `(s -- `(string . ,(read s))))
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
                 (and show
                      (+ (and "," show)))
                 (substring ")"))
           `(items --
                   `(tuple . ,(cons (cadr items)
                                    (caddr items)))))
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
          (field-name (substring [a-z] (* [A-Z a-z 0-9 ?_ ?'])))))
       (tests `(("A" (data . ("A" . ())))
                ("Bar" (data . ("Bar" . ())))
                ("Bar (Mu)" (data . ("Bar" . ((data . ("Mu" . ()))))))
                ("1" (num . 1))
                ("1.1" (num . 1.1))
                ("\"string\""  (string . "string"))
                ("\"st\\\"ring\"" (string . "st\"ring"))
                ("[6,9]" (list . ((num . 6) (num . 9))))
                ("[6,[A,B]]" (list . ((num . 6) (list . ((data . ("A" . ())) (data . ("B" . ())))))))
                ("[A,B]" (list . ((data . ("A" . ())) (data . ("B" . ())))))
                ("(1,2)" (tuple . ((num . 1) . (num . 2))))
                ("[(1,2),(3,4)]" (list . ((tuple . ((num . 1) . (num . 2)))
                                          (tuple . ((num . 3) . (num . 4))))))
                ("Foo {fooX = 1}" (record . ("Foo" . (("fooX" . (num . 1))))))
                ("Foo {fooX = 1, fooY_ = A, fooZ1 = Foo, fooA' = [1,2], fooB = (1,2), fooC = [A,(B,\"C\")]}"
                 (record . ("Foo" .
                            (("fooX" . (num . 1))
                             ("fooY_" . (data . ("A" . ())))
                             ("fooZ1" . (data . ("Foo" . ())))
                             ("fooA'" . (list . ((num . 1) (num . 2))))
                             ("fooB" . (tuple . ((num . 1) . (num . 2))))
                             ("fooC" . (list . ((data . ("A" . ()))
                                                (tuple . ((data . ("B" . ())) .
                                                          (string . "C"))))))))))))
       (hs-show-pretty
        (lambda (tree &optional parens)
          (case (car tree)
            ('list (format "[%s]"
                           (mapconcat
                            (lambda (x)
                              (funcall hs-show-pretty x))
                            (cdr tree)
                            ",")))
            ('record (let ((record (cdr tree)))
                       (format "%s%s {%s}%s"
                               (if parens "(" "")
                               (car record)
                               (mapconcat (lambda (field)
                                            (format "%s = %s"
                                                    (car field)
                                                    (funcall hs-show-pretty (cdr field))))
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
                                        (lambda (x) (funcall hs-show-pretty x t))
                                        (cdr data)
                                        " ")))
                             (if parens ")" ""))))
            (otherwise (format "MISSING: %S" tree)))))
       (hs-insert-pretty
        (lambda (column tree &optional parens)
          (case (car tree)
            ('list (let ((start (point)))
                     (insert "[")
                     (mapcar/i (lambda (x i len)
                                 (funcall hs-insert-pretty (+ column 1) x)
                                 (unless (> i (- len 2))
                                   (insert (concat ",\n" (indent (+ 1 column) "")))
                                   (put-text-property (1- (point)) (point) 'face 
                                                      '(foreground zenburn-red))))
                               (cdr tree))
                     (insert "]")))
            ('record
             (let ((record (cdr tree)) (overlay (list 'nil)))
               (insert (if parens "(" ""))
               (let ((link-start (point)))
                 (insert (car record))
                 (let ((button (make-text-button link-start (point))))
                   (put-text-property link-start (point) 'face 
                                      '(foreground font-lock-type-face))
                   (button-put button 'overlay overlay)
                   (button-put
                    button
                    'action
                    (lambda (btn)
                      (let ((overlay (button-get btn 'overlay)))
                        (when overlay
                          (overlay-put (car overlay) 
                                       'invisible (not (overlay-get (car overlay)
                                                                    'invisible)))))))))
               (insert " {\n")
               (let ((curly-start (1- (point))))
                 (mapcar/i (lambda (field i len)
                             (insert (indent column (car field)))
                             (insert " = ")
                             (put-text-property (- (point) 3) (point) 'face
                                                '(foreground font-lock-constant-face))
                             (funcall hs-insert-pretty
                                      (+ (length (car field)) column 3)
                                      (cdr field))
                             (unless (> i (- len 2))
                               (insert ",\n")
                               (put-text-property (1- (point)) (point) 'face
                                                  '(foreground zenburn-green))))
                           (cdr record))
                 (insert (concat "\n" (indent column "}")))
                 (progn
                   (setf (car overlay) (make-overlay curly-start (- (point) 1) nil t))
                   (overlay-put (car overlay) 'invisible t)
                   )
                 (insert (if parens ")" "")))))
            ('num (let ((num-start (point)))
                    (insert (format "%d" (cdr tree)))
                    (put-text-property num-start (point) 'face 
                                       '(foreground font-lock-constant-face))))
            ('string (let ((str-start (point)))
                       (insert (format "%S" (cdr tree)))
                       (put-text-property str-start (point) 'face 
                                          '(foreground font-lock-string-face))))
            ('data (let ((data (cdr tree)))
                     (insert (if parens "(" ""))
                     (let ((cons-start (point)))
                       (insert (car data))
                       (put-text-property cons-start (point) 'face 
                                          '(foreground font-lock-type-face)))
                     (unless (null (cdr data))
                       (progn (insert " ")
                              (mapcar/i
                               (lambda (x i len)
                                 (funcall hs-insert-pretty column x t)
                                 (unless (> i (- len 2))
                                   (insert " ")))
                               (cdr data))))
                     (insert (if parens ")" ""))))
            (otherwise (error "MISSING: %S" tree))))))
  (when nil
    (let ((results
           (remove-if
            (lambda (x) (eq x t))
            (mapcar (lambda (test)
                      (let* ((given (car test))
                             (expected (cadr test))
                             (result (car (eval `(peg-parse-string ,hs-show-parser given)))))
                        (if (equal result expected)
                            t
                          (format "FOR\n\n%s\n\nEXPECTED\n\n%S\n\nBUT GOT\n\n%S"
                                  given expected result))))
                    tests))))
      (if (> (length results) 0)
          (message (car results))
        (message (format "OK, passed %s tests."
                         (length tests))))))
  (let ((orig "[Submission {submissionId = SubmissionId {unSubmissionId = 246684}, submissionTrackId = TrackId {unTrackId = 24}, submissionConferenceId = ConferenceId {unConferenceId = 51728}, submissionUserId = UserId {unUserId = 703883}, submissionDetails = SubDetails {subDetailsTitle = Title {unTitle = \"x\"}, subDetailsAbstract = Abstract {unAbstract = \"You are creating a draft submission. It will not be public. This will give you the opportunity to review and correct any details before you submit.You are creating a draft submission. It will not be public. This will give you the opportunity to review and correct any details before you submit.\"}, subDetailsKeywords = [Keyword {unKeyword = \"x\"}]}, submissionAuthors = [Author {authorId = AuthorId {unAuthorId = 268222}, authorAffiliation = Affiliation {unAffiliation = \"\"}, authorEmail = Email {unEmail = \"chrisdone@gmail.com\"}, authorForename = Forename {unForename = \"Chris\"}, authorMiddle = Just (Middlename {unMiddlename = \"\"}), authorSurname = Surname {unSurname = \"Done\"}, authorWeight = Weight {unWeight = 0}, authorKeywords = [Keyword {keywordWeight = 12, keywordTerm = \"science\"},Keyword {keywordWeight = 5, keywordTerm = \"physics\"}], authorCorresponding = True, authorSubmission = SubmissionId {unSubmissionId = 246684}, authorConference = ConferenceId {unConferenceId = 0}},Author {authorId = AuthorId {unAuthorId = 1234}, authorAffiliation = Affiliation {unAffiliation = \"\"}, authorEmail = Email {unEmail = \"davebobsville@emails.com\"}, authorForename = Forename {unForename = \"Dave\"}, authorMiddle = Just (Middlename {unMiddlename = \"\"}), authorSurname = Surname {unSurname = \"Bobsbille\"}, authorWeight = Weight {unWeight = 0}, authorKeywords = [Keyword {keywordWeight = 12, keywordTerm = \"science\"},Keyword {keywordWeight = 5, keywordTerm = \"physics\"}], authorCorresponding = True, authorSubmission = SubmissionId {unSubmissionId = 246684}, authorConference = ConferenceId {unConferenceId = 0}}], submissionConflicts = [], submissionCorreAuthor = Index {unIndex = 268222}, submissionManuscripts = [Attachment {attachmentId = ManuscriptId {unManuscriptId = 90987}, attachmentFilename = Filename {unFilename = \"1e132_demo-paper-1.pdf\"}, attachmentContentType = ApplicationPDF, attachmentName = \"1e132_demo-paper-1.pdf\"}], submissionState = Submitted, submissionPrelim = None, submissionCheck = UnChecked, submissionCameraReadyOf = Nothing, submissionExternal = False, submissionCopyright = False, submissionTerms = Nothing, submissionCameraReadyUploaded = False, submissionCMCId = Nothing, submissionPaperTypeId = Nothing, submissionValidatedPDF = Nothing, submissionValidated = False, submissionDOI = Nothing}]"))
    (when (string= orig
                   (funcall hs-show-pretty (car (eval `(peg-parse-string ,hs-show-parser orig)))))
                                        ;(message "OK, passed example.")
      
      (let ((buffer (get-buffer "*demo*"))
            (tree (car (eval `(peg-parse-string ,hs-show-parser orig)))))
        (with-current-buffer buffer
          (mapcar 'delete-overlay (overlays-in (point-min) (point-max)))
          (erase-buffer)
                                        ;(message (funcall hs-show-pretty tree))
          (funcall hs-insert-pretty 0 tree))))))
