(add-hook 'c-mode-common-hook '(lambda () (c-toggle-auto-state 1)))

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Font configuration
(set-face-attribute 'default nil :height 110)

; Packages
;; Tell emacs where is your personal elisp lib dir
;; this is the dir you place all your extra packages
(let ((default-directory "~/.emacs.d/elisp/"))
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path))
(require 'yasnippet)
(setq yas/snippet-dirs '("~/.emacs.d/snippets" "~/.emacs.d/elisp/yasnippet/snippets"))
(yas/global-mode 1) 

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Dropbox/GTD/newgtd.org")))
 '(custom-theme-directory "~/.emacs.d/elisp/emacs-color-theme-solarized")
 )


;; Evil
(require 'evil)
(evil-mode 1)

; Tramp
(require 'tramp)
(setq tramp-default-method "scp")

; Octave mode
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

; Haskell
(setq haskell-program-name "/home/tcebrian/GHC/bin/ghci")

; IDO Mode
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

; Hide the splash screen and banner
(setq inhibit-startup-message t 
inhibit-startup-echo-area-message t)

; Use the system configured browser
;(setq browse-url-browser-function 'browse-url-generic
;      browse-url-generic-program "google-chrome")

; Emacs autoindent
(define-key global-map (kbd "RET") 'newline-and-indent)

; No backup
(setq make-backup-files nil)

;;; Org mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)
(setq org-log-done 'time) ; Close tasks with timestamp

; Timer for the Pomodor Technique
(setq org-timer-default-timer 25)
(add-hook 'org-clock-in-hook '(lambda () 
      (if (not org-timer-current-timer) 
      (org-timer-set-timer '(16)))))

; Add reference search in Org mode
(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
	 ;enable auto-revert-mode to update reftex when bibtex file changes on disk
	 (global-auto-revert-mode t)
	 (reftex-parse-all)
	 ;add a custom reftex cite format to insert links
	 (reftex-set-cite-format
	  '((?b . "[[bib:%l][%l-bib]]")
	    (?n . "[[notes:%l][%l-notes]]")
	    (?p . "[[papers:%l][%l-paper]]")
	    (?t . "%t")
	    (?h . "** %t\n:PROPERTIES:\n:Custom_ID: %l\n:END:\n[[papers:%l][%l-paper]]")))))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)

; Org mode pomodoro
;(add-to-list 'org-modules 'org-timer)
;(setq org-timer-default-timer 25)
;(add-hook 'org-clock-in-hook '(lambda () 
;      (if (not org-timer-current-timer) 
;      (org-timer-set-timer '(16)))))

(add-hook 'message-mode-hook 'turn-on-flyspell 'append)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

; Archive all the tasks in a subtree
(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/CANCELLED" 'file)
  (org-map-entries 'org-archive-subtree "/DONE" 'file)
  )

; Agenda commands
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-custom-commands 
  '(("H" "Office and Home Lists" ((agenda)
				  (tags-todo "COMPUTER")
				  (tags-todo "READING")
				  (tags-todo "HOME")
				  (tags-todo "OFFICE")))
    ("O" "Office Lists" ((agenda)
				  (tags-todo "OFFICE")))
  ("D" "Daily Action List"
      (
           (agenda "" ((org-agenda-ndays 1)
                       (org-agenda-sorting-strategy
                        (quote ((agenda time-up priority-down tag-up) )))
                       (org-deadline-warning-days 0)
                       ))))
  )
)

; Templates
(setq org-capture-templates
  '(("t" "Todo" entry (file+headline "~/Dropbox/GTD/newgtd.org" "Tasks")
             "* TODO %?\n  %i\n  %a")
   ("j" "Journal" entry (file+datetree "~/Dropbox/GTD/journal.org")
        "* %?\nEntered on %U\n  %i\n  %a")))

; Setting up org-capture
(setq org-directory "~/Dropbox/GTD")
(setq org-default-notes-file (concat org-directory "/notes.org"))

; Color theme
; (load-theme 'tango-dark)
(package-initialize)
(load-theme 'solarized-dark t)

; Recent files opened support
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(put 'upcase-region 'disabled nil)

; Asciidoc configuration
(autoload 'adoc-mode "adoc-mode")
(add-to-list 'auto-mode-alist '("\\.acd$" . adoc-mode))

; Kill this buffer and the window with C-c k
(substitute-key-definition 'kill-buffer 'kill-buffer-and-window global-map)

; Move to windows with cursors
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <down>")  'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>")  'windmove-left)

; Org-babel configuration
(require 'ob-haskell)
(require 'ob-dot)
(require 'ob-python)
(require 'ob-sh)
(require 'ob-sql)
(require 'ob-gnuplot)
(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)

; Use ipython as the inferior interpreter
(require 'python-mode)

; Rectangular visual selection
(require 'rect-mark)
(global-set-key (kbd "C-x r C-SPC") 'rm-set-mark)
(global-set-key (kbd "C-x r C-x")   'rm-exchange-point-and-mark)
(global-set-key (kbd "C-x r C-w")   'rm-kill-region)
(global-set-key (kbd "C-x r M-w")   'rm-kill-ring-save)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
