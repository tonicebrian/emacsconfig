(add-hook 'c-mode-common-hook '(lambda () (c-toggle-auto-state 1)))

(add-hook 'text-mode-hook 'turn-on-auto-fill)

; Packages
;; Tell emacs where is your personal elisp lib dir
;; this is the dir you place all your extra packages
(let ((default-directory "~/.emacs.d/elisp/"))
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path))

(require 'evernote-mode)

(global-linum-mode 1)

; IDO Mode
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

; Hide the splash screen and banner
(setq inhibit-startup-message t 
inhibit-startup-echo-area-message t)

; Use the system configured browser
(setq browse-url-generic-program "x-www-browser")

; Emacs autoindent
(define-key global-map (kbd "RET") 'newline-and-indent)

; No backup
(setq make-backup-files nil)

; Org mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)
(setq org-log-done 'time) ; Close tasks with timestamp

; Archive all the tasks in a subtree
(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

; Agenda commands
(setq org-agenda-custom-commands 
'(("H" "Office and Home Lists"
     ((agenda)
          (tags-todo "OFFICE")
          (tags-todo "HOME")
          (tags-todo "COMPUTER")
          (tags-todo "DVD")
          (tags-todo "READING")))
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
(load-theme 'tango-dark)

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
(setq org-src-fontify-natively t)

; el-get for package management
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)
(let ((default-directory "~/.emacs.d/el-get/"))
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path))

; Rectangular visual selection
(require 'rect-mark)
(global-set-key (kbd "C-x r C-SPC") 'rm-set-mark)
(global-set-key (kbd "C-x r C-x")   'rm-exchange-point-and-mark)
(global-set-key (kbd "C-x r C-w")   'rm-kill-region)
(global-set-key (kbd "C-x r M-w")   'rm-kill-ring-save)

