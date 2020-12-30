;; Install plugins:
;; use-package, color-theme-approximate, flycheck, magit,
;; aggressive-indent, emmet-mode

;; Required for all plugins
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Auto-complete
;; (ac-config-default)
;; (auto-complete-mode t)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Aggresive indent
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)

;; Resize buffers
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Move conveniently between buffers
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; Miscelaneous
(xterm-mouse-mode 1)
(setq make-backup-files nil)
(setq auto-save-default nil)
(load-theme 'wheatgrass t)
(color-theme-approximate-on)
(delete-selection-mode 1)
(electric-pair-mode 1)
(load-theme 'wheatgrass t)
(ido-mode 1)
(desktop-save-mode 1)
(save-place-mode t)
(blink-cursor-mode 0) ; gui
(setq scroll-step 1)
(setq ring-bell-function 'ignore)
(menu-bar-mode -1)
(tool-bar-mode -1)
(fringe-mode 0)
(setq completion-ignore-case  t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(set-frame-font "roboto mono-12" nil t)
(setq column-number-mode t)
(show-paren-mode 1)
(global-hl-line-mode 1)
(set-face-attribute 'hl-line nil :inherit nil :background "#303030")
(c-set-offset 'case-label '+) ;; switch indentation
;; 80-column limit (optional)
;; (setq-default auto-fill-function 'do-auto-fill)
;; (setq-default fill-column 80)

(custom-set-variables
 '(c-basic-offset 4)
 '(css-indent-offset 2)
 '(js-indent-level 2)
 '(perl-indent-level 2)
 '(python-indent-offset 4)
 '(package-selected-packages
   (quote
    (emmet-mode color-theme-approximate magit aggressive-indent flycheck))))

;; Highlight TODO, FIXME, etc
(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode python-mode css-mode js-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-note-face)
(mapc (lambda (mode)
	(font-lock-add-keywords
	 mode
	 '(("\\<\\(TODO\\|FIXME\\|BUG\\)" 1 'font-lock-fixme-face t)
	   ("\\<\\(NOTE\\|WORKAROUND\\|IMPROVEMENT\\)" 1 'font-lock-note-face t))))
      fixme-modes)
(modify-face 'font-lock-fixme-face "#cc1c1c" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "#0c8e17" nil nil t nil t nil nil)

;; Emmet mode
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode)

;; Compile
(setq compile-command "compile.bat")
(add-hook 'c-mode-common-hook
	  (lambda()
	    (define-key c-mode-base-map (kbd "C-c l") ' compile)))

;; Find in files using findstr (win32 only)
(when (eq system-type 'windows-nt)
  (with-eval-after-load 'grep
    ;; findstr can handle the basic find|grep use case
    (grep-apply-setting 'grep-find-template
                        "findstr /S /N /D:. /C:<R> <F>")
    (setq find-name-arg nil)))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
