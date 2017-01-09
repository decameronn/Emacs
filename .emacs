; This is my first attempt at configuring Emacs
; Bellow are settings and keyboard shortucts
; Also using some settings created by Casey Muratori, (CM)


; Stop Emacs from losing undo information by
; setting very high limits for undo buffers (CM)
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

; Turn off toolbar
(tool-bar-mode 0)

; Show system clock
(display-time)

; Define fonts and colors
(set-foreground-color "#f7f5f5")
(set-background-color "#565656")
(set-face-attribute 'default nil :font "Liberation Mono" )
(set-frame-font "Liberation Mono" nil t)

; Set custom Frame Size at StartUp
(add-to-list 'default-frame-alist '(height . 55))
(add-to-list 'default-frame-alist '(width . 180))

; Toggle line numbers in the editor
; if the text is C/C++ file
(defun my-c-mode-common-hook ()
  (line-number-mode 1))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

; Turn off system bell on Mac OS X (CM)
(defun nil-bell ())
(setq ring-bell-function 'nil-bell)

; Bright-red TODOs (CM)
 (setq fixme-modes '(c++-mode c-mode emacs-lisp-mode))
 (make-face 'font-lock-fixme-face)
 (make-face 'font-lock-note-face)
 (mapc (lambda (mode)
     (font-lock-add-keywords
      mode
      '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
            ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
    fixme-modes)
 (modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
 (modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)

; Smooth scroll (CM)
(setq scroll-step 3)

; Accepted file extensions and their appropriate modes (CM)
(setq auto-mode-alist
      (append
       '(("\\.cpp$"    . c++-mode)
         ("\\.hin$"    . c++-mode)
         ("\\.cin$"    . c++-mode)
         ("\\.inl$"    . c++-mode)
         ("\\.rdc$"    . c++-mode)
         ("\\.h$"    . c++-mode)
         ("\\.c$"   . c++-mode)
         ("\\.cc$"   . c++-mode)
         ("\\.c8$"   . c++-mode)
         ("\\.txt$" . indented-text-mode)
         ("\\.emacs$" . emacs-lisp-mode)
         ("\\.gen$" . gen-mode)
         ("\\.ms$" . fundamental-mode)
         ("\\.m$" . objc-mode)
         ("\\.mm$" . objc-mode)
         ) auto-mode-alist))

; C++ indentation style (CM)
(defconst casey-big-fun-c-style
  '((c-electric-pound-behavior   . nil)
    (c-tab-always-indent         . t)
    (c-comment-only-line-offset  . 0)
    (c-hanging-braces-alist      . ((class-open)
                                    (class-close)
                                    (defun-open)
                                    (defun-close)
                                    (inline-open)
                                    (inline-close)
                                    (brace-list-open)
                                    (brace-list-close)
                                    (brace-list-intro)
                                    (brace-list-entry)
                                    (block-open)
                                    (block-close)
                                    (substatement-open)
                                    (statement-case-open)
                                    (class-open)))
    (c-hanging-colons-alist      . ((inher-intro)
                                    (case-label)
                                    (label)
                                    (access-label)
                                    (access-key)
                                    (member-init-intro)))
    (c-cleanup-list              . (scope-operator
                                    list-close-comma
                                    defun-close-semi))
    (c-offsets-alist             . ((arglist-close         .  c-lineup-arglist)
                                    (label                 . -4)
                                    (access-label          . -4)
                                    (substatement-open     .  0)
                                    (statement-case-intro  .  4)
                                    (statement-block-intro .  c-lineup-for)
                                    (case-label            .  4)
                                    (block-open            .  0)
                                    (inline-open           .  0)
                                    (topmost-intro-cont    .  0)
                                    (knr-argdecl-intro     . -4)
                                    (brace-list-open       .  0)
                                    (brace-list-intro      .  4)))
    (c-echo-syntactic-information-p . t))
"Casey's Big Fun C++ Style")

; Split Emacs horizontally at StartUp
(setq split-height-threshold nil)
(setq split-width-threshold 160)

; Split Emacs horizontally at StartUp, and create a new buffer (CM)
(setq next-line-add-newlines nil)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)
(split-window-horizontally)

; Define 4-sized General Tab
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

; Define 4-sized C/C++ Tab
(setq-default c-basic-offset 4)

