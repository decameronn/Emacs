(require 'package)
(package-initialize)

;; (split-window-horizontally)

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; A better C-x C-f
(load-library "view")
(require 'cc-mode)
(require 'ido)
(require 'compile)
(ido-mode t)

;; Miscelaneous
(blink-cursor-mode 0)
(setq scroll-step 3)
(setq ring-bell-function 'ignore)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Neotree
(add-to-list 'load-path "../git/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-window-fixed-size nil)

;; ERC
(setq erc-server "")
(setq erc-port "")
(setq erc-nick "")  
(setq erc-password "")
(setq erc-user-full-name "")

;; Custom Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-file "~/.emacs.d/themes/manoj-dark-theme.el")
;; Custom Font
(add-to-list 'default-frame-alist '(font . "Consolas-13" ))
(set-face-attribute 'default t :font "Consolas-13" )

;; Frame configuration at startup: fullscreen & custom size
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(width . 180))
;; (add-to-list 'default-frame-alist '(height . 45))

;; C++ begin
(setq-default c-basic-offset 4)
(setq c-default-style "bsd")

(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key (kbd "C-c m d") 'ff-find-other-file)))

(global-hl-line-mode 1)

(setq column-number-mode t)

(ac-config-default)
(global-auto-complete-mode t)
(add-hook 'c++-mode
  (lambda ()
    (add-to-list 'ac-sources 'ac-source-semantic)))

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

(show-paren-mode 1)

(setq compile-command "build.bat")
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

;; 80 column ruler
;; (require 'fill-column-indicator)
;; (setq fci-rule-column 80)
;; (setq fci-rule-width 1)
;; (setq fci-rule-color "#121212")

;; C++ End

