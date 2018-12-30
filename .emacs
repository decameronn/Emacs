;; Required for all plugins
(require 'package)
(package-initialize)
	     
(ac-config-default)
(global-auto-complete-mode t)
(add-hook 'c++-mode
  (lambda ()
    (add-to-list 'ac-sources 'ac-source-semantic)))
    
;; Move conveniently between buffers
;; (global-set-key (kbd "C-x <up>") 'windmove-up)
;; (global-set-key (kbd "C-x <down>") 'windmove-down)
;; (global-set-key (kbd "C-x <right>") 'windmove-right)
;; (global-set-key (kbd "C-x <left>") 'windmove-left)

;; A better file browser
(load-library "view")
(require 'cc-mode)
(require 'ido)
(require 'compile)
(ido-mode t)

;; Miscelaneous
(blink-cursor-mode 0)
(setq scroll-step 1)
(setq ring-bell-function 'ignore)
(menu-bar-mode -1)
(tool-bar-mode -1)
;;(split-window-horizontally)
;;(setq-default line-spacing 0)

;; 80-column limit
(setq-default auto-fill-function 'do-auto-fill)
(setq-default fill-column 80)

;; Evil
(require 'evil)
  (evil-mode 1)

;; ERC
(setq erc-server "")
(setq erc-port "")
(setq erc-nick "")  
(setq erc-password "")
(setq erc-user-full-name "")

;; Custom Theme & Font
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;;(load-file "~/.emacs.d/themes/deep-thought.el")
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono 14" ))
(set-face-attribute 'default t :font "DejaVu Sans Mono 14" )

;; Frame configuration at startup
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))
;;(add-to-list 'default-frame-alist '(width . 180))
;;(add-to-list 'default-frame-alist '(height . 45))

;; C++ begin
(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key (kbd "C-c m d") 'ff-find-other-file)))

(setq-default c-basic-offset 8)
(setq c-default-style "bsd")
(setq column-number-mode t)
;; (show-paren-mode 1)
;; (global-hl-line-mode 1)
;; (global-linum-mode t)

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))
(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key (kbd "<M-up>")  'move-line-up)
(global-set-key (kbd "<M-down>")  'move-line-down)

(setq compile-command "compile.bat")
(add-hook 'c-mode-common-hook
  (lambda()
    (define-key c-mode-base-map (kbd "C-c l") ' compile)))

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
  If there's no region, the current line will be duplicated. However, if
  there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)
;; C++ End
