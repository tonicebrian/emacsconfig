(add-hook 'c-mode-common-hook '(lambda () (c-toggle-auto-state 1)))

(global-linum-mode 1)

; IDO Mode
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

; Hide the splash screen and banner
(setq inhibit-startup-message t 
inhibit-startup-echo-area-message t)

; Emacs autoindent
(define-key global-map (kbd "RET") 'newline-and-indent)

; No backup
(setq make-backup-files nil)