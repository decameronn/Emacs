;; ********** TIPS, TRICKS, INFO & TROUBLESHOOTING  ********* ;;

;; HOME location under WINDOWS 10:
;; Emacs (terminal): %userprofile%
;; Emacs (GUI): %appdata%

;; A peek at Emacs: Rectangular selection
;; https://emacsredux.com/blog/2014/01/01/a-peek-at-emacs-24-dot-4-rectangular-selection/
;; https://www.emacswiki.org/emacs/RectangleMark
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Rectangles.html

;; Auto-complete Troubleshooting
;; https://emacs.stackexchange.com/questions/10480/cannot-install-auto-complete-package

;; ******** USEFUL COMMANDS ******** ;;
;; Select word: C-M-Space
;; Select between paretheses, brackets, string, etc: C-M-u C-M-Space
;; Zap until character: C-z / M-z CHAR
;; Forward/Backwards until character: C-s / C-r CHAR
;; Delete between mark and character: C-Space C-r CHAR <RET>

;; Rectangles
;; 1. Select desired columns/rectangle (special mark): C-x-Space
;; 2. Select what is needed with C-b and C-n
;; 3. There are multiple options now. All start with C-x r <option>
;;    To mention a few:
;;    C-x r d: delete rectangle
;;    C-x r c: clear rectangle (replace with spaces)
;;    C-x r M-w : copy selected rectangle
;;    C-x r t 'string' : replace rectangle with given string

;; Required for all plugins
(require 'package)
(package-initialize)
	     
;; Auto-complete
(ac-config-default)
(global-auto-complete-mode t)
(add-hook 'c++-mode
  (lambda ()
    (add-to-list 'ac-sources 'ac-source-semantic)))

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

;; Custom Theme
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;;(load-file "~/.emacs.d/themes/manoj-dark-theme.el")
;; Custom Font
(add-to-list 'default-frame-alist '(font . "Iosevka Term 14" ))
(set-face-attribute 'default t :font "Iosevka Term 14" )

;; Frame configuration at startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(width . 180))
;; (add-to-list 'default-frame-alist '(height . 45))

;; Miscelaneous
(blink-cursor-mode 0)
(setq scroll-step 1)
(setq ring-bell-function 'ignore)
;;(menu-bar-mode -1)
;;(tool-bar-mode -1)
(fringe-mode 4)
(setq completion-ignore-case  t)
;;(split-window-horizontally)
;;(setq-default line-spacing 0)
;;(global-hl-line-mode 1)
;;(global-linum-mode t)

;; 80-column limit
(setq-default auto-fill-function 'do-auto-fill)
(setq-default fill-column 80)

;; Backup-files directory
(setq backup-directory-alist `(("." . "~/.emacs.d/backup-files")))

;; Highlight TODO, FIXME, etc
(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode))
(make-face 'font-lock-fixme-face)
(mapc (lambda (mode)
	(font-lock-add-keywords
	 mode
	 '(("\\<\\(TODO\\|FIXME\\|BUG\\)" 1 'font-lock-fixme-face t)
	   ("\\<\\(NOTE\\|WORKAROUND\\|IMPROVEMENT\\)" 1 'font-lock-note-face t))))
      fixme-modes)
(modify-face 'font-lock-fixme-face "#cc1c1c" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "#0c8e17" nil nil t nil t nil nil)

;; Miscelaneous
(blink-cursor-mode 0)
(setq scroll-step 1)
(setq ring-bell-function 'ignore)
;;(menu-bar-mode -1)
;;(tool-bar-mode -1)
(fringe-mode 4)
(setq completion-ignore-case  t)
;;(split-window-horizontally)
;;(setq-default line-spacing 0)
;;(global-hl-line-mode 1)
;;(global-linum-mode t)

;; 80-column limit
(setq-default auto-fill-function 'do-auto-fill)
(setq-default fill-column 80)

;; Backup-files directory
(setq backup-directory-alist `(("." . "~/.emacs.d/backup-files")))

;; Highlight TODO, FIXME, etc
(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode))
(make-face 'font-lock-fixme-face)
(mapc (lambda (mode)
	(font-lock-add-keywords
	 mode
	 '(("\\<\\(TODO\\|FIXME\\|BUG\\)" 1 'font-lock-fixme-face t)
	   ("\\<\\(NOTE\\|WORKAROUND\\|IMPROVEMENT\\)" 1 'font-lock-note-face t))))
      fixme-modes)
(modify-face 'font-lock-fixme-face "#cc1c1c" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "#0c8e17" nil nil t nil t nil nil)



;; ***** LANGUAGE SPECIFIC CONFIGURATIONS BELOW *** ;;

;; HTML, CSS, JS BEGIN ********************************************************
;; Web-mode: automatically load in related files
;; Activate web-mode & emmet-mode snippets, press C-j
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
;; Set indentations (hook is executed when the mode is turned on)
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-css-indent-offset 4))
(add-hook 'web-mode-hook  'my-web-mode-hook)    
(setq tab-width 4)
;; Company-mode settings (css & html completions in web-mode)
(defun my-web-mode-hook ()
  (set (make-local-variable 'company-backends) 
       '(company-css company-web-html company-yasnippet company-files)))
;; Turn on Emmet in web-mode & toggle corresponding mode switch (file type)
(add-hook 'web-mode-hook  'emmet-mode)
(add-hook 'web-mode-before-auto-complete-hooks
    '(lambda ()
     (let ((web-mode-cur-language
  	    (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "php")
    	   (yas-activate-extra-mode 'php-mode)
      	 (yas-deactivate-extra-mode 'php-mode))
               (if (string= web-mode-cur-language "css")
    	   (setq emmet-use-css-transform t)
      	 (setq emmet-use-css-transform nil))))) 
;; HTML, CSS, JS END **********************************************************

;; C/C++ BEGIN ****************************************************************
(setq-default c-basic-offset 4)
(setq c-default-style "bsd")
(c-set-offset 'case-label '+) ;; switch indentation
(setq column-number-mode t)
(show-paren-mode 1)

(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key (kbd "C-c m d") 'ff-find-other-file)))

(setq compile-command "compile.bat")
(add-hook 'c-mode-common-hook
  (lambda()
    (define-key c-mode-base-map (kbd "C-c l") ' compile)))

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
;; C/C++ END *****************************************************************

;; ERC
(setq erc-port "")
(setq erc-nick "")  
(setq erc-password "")
(setq erc-user-full-name "")


;; ************* UNUSED PLUGINS *************** ;;
;; Evil-mode
;;(require 'evil)
;;  (evil-mode 1)
