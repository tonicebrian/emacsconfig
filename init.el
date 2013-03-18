(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit
                      starter-kit-bindings
                      starter-kit-js
                      autopair
                      yasnippet
                      auto-complete
                      color-theme-sanityinc-solarized
                      recentf
                      evil
                      fuzzy)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; autopair and yas in all modes
(autopair-global-mode)
(yas-global-mode 1)

;; autocomplete
(require 'auto-complete-config)
(setq ac-dictionary-files (list (concat user-emacs-directory ".dict")))
(ac-config-default)
;; hack to fix ac-sources after pycomplete.el breaks it
(add-hook 'python-mode-hook
          '(lambda ()
             (setq ac-sources '(ac-source-pycomplete
                                ac-source-abbrev
                                ac-source-dictionary
                                ac-source-words-in-same-mode-buffers))))

;; Set up python-mode
(setq py-install-directory (concat esk-user-dir "/python-mode.el-6.0.12/"))
(add-to-list 'load-path py-install-directory)
;; this will show method signatures while typing
(setq py-set-complete-keymap-p t)
(require 'python-mode)
;; activate the virtualenv where Pymacs is located
(virtualenv-workon "default/")

(defun load-pycomplete ()
  "Load and initialize pycomplete."
  (interactive)
  (let* ((pyshell (py-choose-shell))
         (path (getenv "PYTHONPATH")))
    (setenv "PYTHONPATH" (concat
                          (expand-file-name py-install-directory) "completion"
                          (if path (concat path-separator path))))
    (if (py-install-directory-check)
        (progn
          (setenv "PYMACS_PYTHON" (if (string-match "IP" pyshell)
                                      "python"
                                    pyshell))
          (autoload 'pymacs-apply "pymacs")
          (autoload 'pymacs-call "pymacs")
          (autoload 'pymacs-eval "pymacs")
          (autoload 'pymacs-exec "pymacs")
          (autoload 'pymacs-load "pymacs")
          (load (concat py-install-directory "completion/pycomplete.el") nil t)
          (add-hook 'python-mode-hook 'py-complete-initialize))
      (error "`py-install-directory' not set, see INSTALL"))))
(eval-after-load 'pymacs '(load-pycomplete))

;; pyflakes flymake integration
;; http://stackoverflow.com/a/1257306/347942
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pycheckers" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
(add-hook 'python-mode-hook 'flymake-mode)

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

; Org mode configuration
;;; Org mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)
(setq org-log-done 'time) ; Close tasks with timestamp

; Archive all the tasks in a subtree
(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/CANCELLED" 'file)
  (org-map-entries 'org-archive-subtree "/DONE" 'file)
  )

; Org-babel configuration
(require 'ob-haskell)
(require 'ob-dot)
(require 'ob-python)
(require 'ob-sh)
(require 'ob-sql)
(require 'ob-gnuplot)
(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)

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
             "* TODO %?\n %i\n %a")
   ("j" "Journal" entry (file+datetree "~/Dropbox/GTD/journal.org")
        "* %?\nEntered on %U\n %i\n %a")))

; Setting up org-capture
(setq org-directory "~/Dropbox/GTD")
(setq org-default-notes-file (concat org-directory "/notes.org"))
