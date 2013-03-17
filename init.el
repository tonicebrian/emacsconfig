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

(load-theme 'sanityinc-solarized-dark t)
(require 'evil)
(evil-mode 1)
