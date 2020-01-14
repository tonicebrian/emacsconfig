(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(
                      adoc-mode
                      auto-complete
                      autopair
                      color-theme-sanityinc-solarized
                      ein
                      evil
                      evil-surround
                      flycheck
                      fuzzy
                      haskell-mode
                      linum-relative
                      lsp-mode
                      lsp-ui
                      lsp-haskell
                      markdown-mode
                      org
                      w3m
                      yasnippet
                      )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(global-flycheck-mode)

(require 'linum-relative)

;; Allow wrapping of text in characters
(require 'evil-surround)
(global-evil-surround-mode 1)

;; LSP Configuration
(require 'lsp)
(require 'lsp-haskell)
(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'haskell-mode-hook 'flycheck-mode)
(add-hook 'haskell-mode-hook #'lsp)
(setq lsp-prefer-flymake nil)
(setq lsp-haskell-process-path-hie "hie-wrapper")

(add-hook 'text-mode-hook '(lambda () (set-fill-column 80)))

; Ensime
;; Load the ensime lisp code...
; (add-to-list 'load-path "~/.emacs.d/ensime/elisp/")
; (require 'ensime)

;; autopair and yas in all modes
(autopair-global-mode)
(yas-global-mode 1)
(setq yas/root-directory "~/.emacs.d/snippets")
(yas/load-directory yas/root-directory)

;; autocomplete
(require 'auto-complete-config)
(setq ac-dictionary-files (list (concat user-emacs-directory ".dict")))
(ac-config-default)

;; menu bar is useful when getting started
(menu-bar-mode)
(setq-default default-tab-width 4)

; Turn off the graphical toolbar
(tool-bar-mode -1)

; Hide the splash screen and banner
(setq inhibit-startup-message t
inhibit-startup-echo-area-message t)

; Emacs autoindent
(define-key global-map (kbd "RET") 'newline-and-indent)

; No backup
(setq make-backup-files nil)

;; Font configuration
(set-face-attribute 'default nil :height 110)

;; Themes
(load-theme 'sanityinc-solarized-dark t)

;; Vim emulation
(require 'evil)
(evil-mode 1)

;; recentf stuff
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

; Kill this buffer and the window with C-c k
(substitute-key-definition 'kill-buffer 'kill-buffer-and-window global-map)

; Move to windows with cursors
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

; Week starts on Monday
(setq calendar-week-start-day 1)

; Org mode configuration
;;; Org mode
(require 'org)
(setq org-journal-dir "~/Dropbox/journal/")

;; For Google Calendar syncrhonization
(setq org-caldav-url "https://www.google.com/calendar/dav")
(setq org-caldav-calendar-id "toni.cebrian@gmail.com")
(setq org-caldav-inbox "~/Dropbox/GTD/google-calendar-inbox.org")
(setq org-caldav-files '("~/Dropbox/GTD/inbox.org"))

;; MobileOrg configuration
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull "~/Dropbox/MobileOrg/inbox-mobile.org")


(setq org-enforce-todo-dependencies t)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)
(setq org-log-done 'time) ; Close tasks with timestamp

(add-hook 'org-mode-hook
          (lambda () (org-indent-mode t)))

; Archive all the tasks in a subtree
(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/CANCELLED" 'file)
  (org-map-entries 'org-archive-subtree "/DONE" 'file)
  )

; Agenda commands
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-start-on-weekday nil)
(setq org-deadline-warning-days 2)
(setq org-agenda-todo-ignore-scheduled t)
(setq org-agenda-todo-ignore-with-date t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "CANCELLED(c)")))
(setq org-tag-alist
      '(("OFFICE" . ?o) ("HOME" . ?h) ("DOING" . ?d) ("ERRANDS" . ?e) ("HOY" . ?t)))

(setq org-agenda-custom-commands
  `(("D" "Daily Action List"
     (
      (agenda "" ((org-agenda-ndays 1)
                  (org-agenda-sorting-strategy (quote ((agenda time-up priority-down tag-up))))
                  (org-deadline-warning-days 0)
                  ))
      (tags-todo "+DOING/!NEXT" ((org-tags-match-list-sublevels t)
                    (org-agenda-overriding-header "NEXT actions in the DOING projects:")
                    (org-agenda-tags-todo-honor-ignore-options t))
                 )
      (tags-todo "+DOING/!TODO" ((org-tags-match-list-sublevels t)
                    (org-agenda-overriding-header "TODO actions in the DOING projects:")
                    (org-agenda-tags-todo-honor-ignore-options t))
                 )

      (tags-todo "-DOING/!NEXT"  (;(org-tags-match-list-sublevels t)
                    (org-agenda-overriding-header "NEXT actions of all the active projects")
                    (org-agenda-tags-todo-honor-ignore-options t))
)
      ))
    ("W" "Weekly view" (
                         (agenda "" ((org-deadline-warning-days 0)))
                         ))
    )
)

(setq org-directory "~/Dropbox/GTD")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-flycheck-enable t)
 '(lsp-ui-sideline-enable t)
 '(org-agenda-files (quote ("~/Dropbox/GTD/gtd.org")))
 '(org-agenda-skip-deadline-if-done t)
 '(org-clock-idle-time 10)
 '(package-selected-packages
   (quote
    (yasnippet w3m lsp-haskell lsp-ui lsp-mode linum-relative fuzzy evil-surround evil ein color-theme-sanityinc-solarized autopair auto-complete adoc-mode))))

(setq org-agenda-file-regexp "\\`[^.].*\\.org'\\|[0-9]+")

; Templates
(setq org-capture-templates
  '(
   ("i" "Idea" entry (file+headline (concat org-directory "/gtd.org") "Capture")
             "* %?\n %i\n %a")
   ("t" "Todo" entry (file+headline (concat org-directory "/gtd.org") "Tasks")
             "* TODO %?\n %i\n %a")
   ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org"))
        "* %?\nEntered on %U\n %i\n %a")))

; Setting up org-capture
(setq org-default-notes-file (concat org-directory "/notes.org"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t (:inherit consolas)))))


;; This file contains local customizations
(if (file-exists-p "~/.emacs.d/local.el") (load "~/.emacs.d/local.el"))
