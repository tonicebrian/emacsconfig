;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Toni Cebrián"
      user-mail-address "toni.cebrian@gmail.com")

(setq ispell-program-name "aspell")


;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
(setq confirm-kill-emacs nil)
(setq calendar-week-start-day 1)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq
   org_notes "~/google-drive/OrgRoam/"
   gtd_folder "~/google-drive/GTD/"
   mendeley_bib "~/Documents/Mendeley Desktop/library.bib"

   default-input-method "spanish-prefix"
   org-directory org_notes
   def-directory org_notes
   org-roam-directory org_notes
   org-roam-index-file "index.org"
   )


(add-hook 'text-mode-hook
   (lambda () (set-input-method "spanish-prefix")))


(setq org-refile-targets (quote (("tickler.org" :maxlevel . 1)
                                 ("gtd.org" :maxlevel . 2)
                                 ("someday.org" :maxlevel . 2))))

(defvar markdown-link-to-org-regexp "s/\[\(.+\)\](\(.+\))/[[\2][\1]]")

;; Show scheduled things in the near term
(after! org
  (setq
   org-startup-with-inline-images t
   org-global-properties
      '(("Effort" . "0 0:10 0:20 0:30 0:45 1:00 1:30 2:00 3:00 4:00 5:00 6:00 8:00"))
   org-image-actual-width nil
   org-todo-keywords '((sequence "NEXT(n)" "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))
   org-tag-alist '(("freelancer" . ?f) ("office" . ?o) ("read" . ?r) ("home" . ?h) ("computer" . ?c) ("doing" . ?d) ("errandS" . ?e) ("HOY" . ?t))
   org-agenda-files (mapcar (lambda (file)(concat gtd_folder file))(list "tickler.org" "gtd.org" "someday.org"))
   org-capture-templates
   '(("t" "Task" entry (file+headline (lambda ()(expand-file-name (concat gtd_folder "gtd.org"))) "Inbox")
      "** TODO %?\n")
     ("p" "Project" entry (file+headline (lambda () (expand-file-name (concat gtd_folder "gtd.org"))) "Projects")
      (file "templates/project.org"))
     ("d" "Daily Review" entry (file (lambda () (expand-file-name (concat (format-time-string "%Y-%m-%d") ".org") org-directory)))
      (file "templates/dailyreview.org"))
     ("w" "Weekly Review" entry (file (lambda () (expand-file-name (concat (format-time-string "%Y-%m-%d") ".org") org-directory)))
      (file "templates/weeklyreview.org"))
     ("m" "Monthly Review" entry (file (lambda () (expand-file-name (concat (format-time-string "%Y-%m-%d") ".org") org-directory)))
      (file "templates/monthlyreview.org"))
     ("a" "Annual Review" entry (file (lambda () (expand-file-name (concat (format-time-string "%Y-%m-%d") ".org") org-directory)))
      (file "templates/annualreview.org"))
   )
  )
  (defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'file))
  ;;
  ;; Show scheduled things in the near term
  (defun my-org-agenda-open-loops ()
  (interactive)
  (let ((org-agenda-start-with-log-mode t))
    (org-agenda-list nil (org-read-date nil nil "-2d") 4)))

  )

(after! org-roam
        (map! :leader
            :prefix "n"
            :desc "org-roam" "l" #'org-roam
            :desc "org-roam-insert" "i" #'org-roam-insert
            :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
            :desc "org-roam-find-file" "f" #'org-roam-find-file
            :desc "org-roam-show-graph" "g" #'org-roam-show-graph
            :desc "org-roam-insert" "i" #'org-roam-insert
            :desc "org-roam-capture" "c" #'org-roam-capture)

        (setq ivy-use-selectable-prompt t)
        (setq org-roam-ref-capture-templates
            '(("r" "ref" plain (function org-roam-capture--get-point)
               "%?"
               :file-name "websites/${slug}"
               :head "#+TITLE: ${title}
    #+ROAM_KEY: ${ref}
    - source :: ${ref}"
               :unnarrowed t))) )


(use-package! org-ref
    ;; :init
    ; code to run before loading org-ref
    :config
    (setq
         org-ref-completion-library 'org-ref-ivy-cite
         org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
         org-ref-default-bibliography (list mendeley_bib)
         org-ref-bibliography-notes (concat org_notes "bibnotes.org")
         org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
         org-ref-notes-directory org_notes
         org-ref-notes-function 'orb-edit-notes
         ))

(after! org-ref
    (setq
     bibtex-completion-notes-path org_notes
     bibtex-completion-bibliography mendeley_bib
     bibtex-completion-pdf-field "file"
     bibtex-completion-notes-template-multiple-files
     (concat
      "#+TITLE: ${title}\n"
      "#+ROAM_KEY: cite:${=key=}\n"
      "* TODO Notes\n"
      ":PROPERTIES:\n"
      ":Custom_ID: ${=key=}\n"
      ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
      ":AUTHOR: ${author-abbrev}\n"
      ":JOURNAL: ${journaltitle}\n"
      ":DATE: ${date}\n"
      ":YEAR: ${year}\n"
      ":DOI: ${doi}\n"
      ":URL: ${url}\n"
      ":END:\n\n"
      )
    )
)

(use-package org-roam-bibtex
  :after (org-roam)
  :bind (:map org-mode-map
         (("C-c n a" . orb-note-actions)))
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq orb-preformat-keywords
   '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "${slug}"
           :head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}

- tags ::
- keywords :: ${keywords}

\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n\n"

           :unnarrowed t))))

(use-package org-noter
  :after (:any org pdf-view)
  :config
  (setq
   ;; The WM can handle splits
   org-noter-notes-window-location 'other-frame
   ;; Please stop opening frames
   org-noter-always-create-frame nil
   ;; I want to see the whole file
   org-noter-hide-other nil
   ;; Everything is relative to the main notes file
   org-noter-notes-search-path (list org_notes)
   )
  )

(use-package org-journal
      :bind
      ("C-c n j" . org-journal-new-entry)
      :custom
      (org-journal-dir org_notes)
      (org-journal-date-prefix "#+TITLE: ")
      (org-journal-file-format "%Y-%m-%d.org")
      (org-journal-date-format "%Y-%m-%d"))
    (setq org-journal-enable-agenda-integration t)

(use-package deft
      :after org
      :bind
      ("C-c n d" . deft)
      :custom
      (deft-recursive t)
      (deft-use-filter-string-for-filename t)
      (deft-default-extension "org")
      (deft-directory org_notes))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Hide markup characters in Org mode for a more pleasant experience
(setq org-hide-emphasis-markers t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
