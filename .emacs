(setq package-list '(auto-complete yasnippet xcscope ecb go-mode py-autopep8 ample-theme drag-stuff imenu-list ggtags flycheck fiplr exec-path-from-shell project-explorer))

;; autopep8 requires you to sudo apt-get install python-autopep8

(require 'package)
; add MELPA to repository list
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
; initialize package.elx
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; disable toolbar
(tool-bar-mode -1)

;; pretty theme
(load-theme 'ample t t)
;;(load-theme 'ample-flat t t)
;;(load-theme 'ample-light t t)
;; choose one to enable
(enable-theme 'ample)

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (load-theme 'ample t t)
              (enable-theme 'ample))))

; TO GET THIS RUN M-x package-install auto-complete
; start auto-complete with emacs
(require 'auto-complete)
; do default config for auto-complete
(require 'auto-complete-config)
(global-auto-complete-mode t)

(set-default 'ac-sources
             '(ac-source-imenu
               ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-words-in-all-buffer))
;; don't ignore case in providing ac matches
(setq ac-ignore-case nil)

;; M-x package-install xscope
(require 'xcscope)

;; Use flycheck whenever we can
;; For python you need to sudo apt-get install pylint
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-temp-prefix ".flycheck")

;;(ac-config-default)

; M-x package-install yasnippet
; yasnippet (gives us templates for loops and functions and stuff)
(require 'yasnippet)
(yas-global-mode 1)

; package-install iedit
; (Weird bug with iedit)
(define-key global-map (kbd "C-c ;") 'iedit-mode)

(define-key global-map (kbd "C-c C-c") 'comment-region)

; package-install ecb
(require 'ecb)


(setq default-frame-alist
      '((top . 250) (left . 400)
        (width . 80) (height . 600)
        (cursor-color . "white")
        (cursor-type . box)
        ;; (foreground-color . "blue")
        ;; (background-color . "white")
        (font . "Monaco 18")
        ;;(font . "DejaVu Sans Mono-18")
        ))
 (set-face-attribute 'default nil :height 230)

;; Use ibuffer instead of regular buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)


(setq initial-frame-alist '((top . 50) (left . 30)))

;; Pyret
(ignore-errors
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/"))
  (require 'pyret)
  (add-to-list 'auto-mode-alist '("\\.arr$" . pyret-mode))
  (add-to-list 'file-coding-system-alist '("\\.arr\\'" . utf-8)))
(add-to-list 'ac-modes 'pyret-mode)


;; Use spaces insteads of tabs
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

;; C
(setq c-default-style "bsd"
      c-basic-offset 4)

(setq show-trailing-whitespace t)


(setq column-number-mode t)
(add-to-list 'default-frame-alist '(height . 55))
(add-to-list 'default-frame-alist '(width . 80))

;; to rename a variable
;; C-c , g (symref) then type in varname
;; C-c C-e (open references)
;; R (capital) to rename 


;; Set M-p and M-n to move cursor up/down 5 lines
(global-set-key (kbd "M-n")
    (lambda () (interactive) (next-line 5)))
(global-set-key (kbd "M-p")
    (lambda () (interactive) (previous-line 5)))

;; ecb key binding stuff
;;(global-set-key (kbd "<M-left>") 'ecb-goto-window-methods)
;;(global-set-key (kbd "<M-right>") 'ecb-goto-window-edit1)

(global-set-key (kbd "<C-M-left>") 'windmove-left)
(global-set-key (kbd "<C-M-right>") 'windmove-right)
(global-set-key (kbd "<C-M-up>") 'windmove-up)
(global-set-key (kbd "<C-M-down>") 'windmove-down)

;; gtags setup
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(define-key ggtags-mode-map (kbd "C-c t s") 'ggtags-find-other-symbol)
;;(define-key ggtags-mode-map (kbd "C-c t d") 'ggtags-find-definition)
;; this bound to M-.
(define-key ggtags-mode-map (kbd "C-c t h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c t r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c t f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c t c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c t u") 'ggtags-update-tags)
(define-key ggtags-mode-map (kbd "C-c t g") 'ggtags-grep)
(define-key ggtags-mode-map (kbd "C-c t n") 'ggtags-navigation-mode-abort)


;; (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

;; hs-minor mode (for collapsing braces)
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(global-set-key (kbd "C-c h h") 'hs-hide-block)
(global-set-key (kbd "C-c h s") 'hs-show-block)
(global-set-key (kbd "C-c h a") 'hs-hide-all)
(global-set-key (kbd "C-c h n") 'hs-show-all)

;; Rust
;; allow autocomplete in rust mode
(add-to-list 'ac-modes 'rust-mode)

;; Go
(require 'go-autocomplete)
(add-to-list 'ac-modes 'go-mode)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))
(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
  )
(add-hook 'go-mode-hook 'my-go-mode-hook)


;; be able to list all functions in buffer
(require 'imenu-list)
(global-set-key (kbd "C-'") #'imenu-list-minor-mode)
(setq imenu-list-focus-after-activation t)



;; cscope
;;(add-hook 'prog-mode-hook #'cscope-minor-mode)


;; colors
(add-to-list 'default-frame-alist '(foreground-color . "#E0DFDB"))
(add-to-list 'default-frame-alist '(background-color . "#102372"))

;; command to create the tags file
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command
   (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))


(setq ibuffer-formats
      '((mark modified read-only " "
              (name 45 45 :left :elide) " "
              (size 9 -1 :right) " "
              (mode 16 16 :left :elide) " " filename-and-process)
        (mark " " (name 16 -1) " " filename)))


;; Whitespace char limit thing
(require 'whitespace)

;; Actually 100 char limit
(setq whitespace-line-column 100)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

;; drag-stuff mode
(drag-stuff-mode t)
(drag-stuff-global-mode 1)
(global-set-key (kbd "C-c h n") 'hs-show-all)

(global-set-key (kbd "C-S-n") 'drag-stuff-down)
(global-set-key (kbd "C-S-p") 'drag-stuff-up)

(global-set-key (kbd "C-c C-;") 'uncomment-region)



;;
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;;
(global-set-key (kbd "C-c f") 'fiplr-find-file)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Switch to other window and make it main window
(defun other-window-kill-buffer ()
  "Kill the other window"
  (interactive)
  (other-window 1)
  (delete-other-windows))
(global-set-key (kbd "C-x K") 'other-window-kill-buffer)


;; OSX ONLY
;;(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
;;(setq exec-path (append exec-path '("/usr/local/bin")))

;; Cool tricks (to print 1, 2, 3, and so on on each line)
;; (dotimes (i 20) (insert (format "%d\n" (1+ i))))

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when (and window-system (eq system-type 'darwin))
  ;; When started from Emacs.app or similar, ensure $PATH
  ;; is the same the user would see in Terminal.app
  (set-exec-path-from-shell-PATH))


;; to get bindings for a command: C-h f
;; to get function for key: C-h k
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#454545" "#cd5542" "#6aaf50" "#baba36" "#5180b3" "#ab75c3" "#68a5e9" "#bdbdb3"])
 '(custom-safe-themes
   (quote
    ("938d8c186c4cb9ec4a8d8bc159285e0d0f07bad46edf20aa469a89d0d2a586ea" default)))
 '(package-selected-packages
   (quote
    (project-explorer go-autocomplete helm-git-grep helm magit fiplr web-beautify flycheck json-reformat yasnippet xcscope py-autopep8 imenu-list go-mode ggtags ecb drag-stuff auto-complete ample-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(setq-default fill-column 79)


(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
(eval-after-load 'sgml-mode
  '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))
(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))


;; git grep
(global-set-key (kbd "C-c g") 'helm-git-grep)
(global-set-key (kbd "C-c C-x g") 'helm-git-grep-at-point)

(define-key helm-map (kbd "M-n")
    (lambda () (interactive) (helm-next-line 5)))
(define-key helm-map (kbd "M-p")
    (lambda () (interactive) (helm-previous-line 5)))


;; project explore
(global-set-key (kbd "C-c p") 'project-explorer-open)
(global-set-key (kbd "C-c C-p C-s") 'project-explorer-helm)

;; C-x SPC to select rectangle
;; C-x r k to kill rectangle
;; C-x r y to yank rectangle

;; registers
;;C-x r <SPC> r
;; Record the position of point and the current buffer in register r (point-to-register). 
;; C-x r j r
;; Jump to the position and buffer saved in register r (jump-to-register).

;; To find source for a library (or package) do M-x find-library (then enter package name like: helm-git-grep or project-explorer)


;; Make helm-git-grep remember the last thing we entered
(setq ian-helm-git-grep-default-text "")

(defun update-last-search (beg end len)
  (if (string= (symbol-name this-command) "self-insert-command")
      (let ((contents (minibuffer-contents)))
        (setq ian-helm-git-grep-default-text contents))
    nil
    ))

(defun my-gg ()
  (interactive)
  ;; Adds the hook before calling the block, and removes it after.
  (minibuffer-with-setup-hook
      (lambda ()
        (insert ian-helm-git-grep-default-text)
        (add-hook 'after-change-functions #'update-last-search)
        )
    (call-interactively 'helm-git-grep))
 (remove-hook 'after-change-functions #'update-last-search))
