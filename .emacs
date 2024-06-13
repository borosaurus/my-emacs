(setq package-check-signature nil)

(setq package-list '(flycheck-ycmd company company-ycmd yasnippet xcscope go-mode py-autopep8 ample-theme drag-stuff imenu-list ggtags flycheck fiplr exec-path-from-shell project-explorer go-autocomplete clang-format wgrep))

;; autopep8 requires you to sudo apt-get install python-autopep8

(require 'package)
; add MELPA to repository list
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

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

;; Turn off gui dialog boxes
(setq use-dialog-box nil)

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (load-theme 'ample t t)
              (enable-theme 'ample))))

;;
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.5)

;; you complete me
(require 'ycmd)
; new
;; (add-hook 'after-init-hook #'global-ycmd-mode)
; old
; (add-hook 'c++-mode-hook #'global-ycmd-mode)

; Don't do semantic completion by default (faster).
;; (set-variable 'ycmd-force-semantic-completion nil)
; make ycmd requests async (faster)
;; (setq company-ycmd-request-sync-timeout 0)

; Special function to force semantic completion.
(defun company-ycmd-semantic-complete ()
  (interactive)
  (let ((ycmd-force-semantic-completion t))
    (company-complete)))
(global-set-key [C-tab] 'company-ycmd-semantic-complete)
(set-variable 'ycmd-server-command `("python3" ,(file-truename "/home/ianboros/ycmd/ycmd/")))

;; treat .h files as C++ (instead of C)
(add-to-list 'auto-mode-alist '("\\.[h]\\'" . c++-mode))

;; ycmd integration with company
(require 'company-ycmd)
(company-ycmd-setup)

(require 'flycheck-ycmd)
(flycheck-ycmd-setup)

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
; (require 'yasnippet)
; (yas-global-mode 1)

; package-install iedit
; (Weird bug with iedit)
(define-key global-map (kbd "C-c ;") 'iedit-mode)

(define-key global-map (kbd "C-c C-c") 'comment-region)

(setq default-frame-alist
      '((top . 250) (left . 400)
        (width . 80) (height . 600)
        (cursor-color . "white")
        (cursor-type . box)
        ;; (foreground-color . "blue")
        ;; (background-color . "white")
        ;;(font . "Monaco 18")
        (font . "DejaVu Sans Mono-18")
        ))

;; Use ibuffer instead of regular buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)


(setq initial-frame-alist '((top . 50) (left . 30)))

;; Pyret
;; (ignore-errors
;;   (add-to-list 'load-path (expand-file-name "~/.emacs.d/"))
;;   (require 'pyret)
;;   (add-to-list 'auto-mode-alist '("\\.arr$" . pyret-mode))
;;   (add-to-list 'file-coding-system-alist '("\\.arr\\'" . utf-8)))


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

(global-set-key (kbd "<C-left>") 'windmove-left)
(global-set-key (kbd "<C-right>") 'windmove-right)
(global-set-key (kbd "<C-up>") 'windmove-up)
(global-set-key (kbd "<C-down>") 'windmove-down)

;; gtags setup
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1)
              (smartparens-mode)
              )))


(define-key ggtags-mode-map (kbd "C-c t s") 'ggtags-find-other-symbol)
;;(define-key ggtags-mode-map (kbd "C-c t d") 'ggtags-find-definition)
;; this bound to M-.
(define-key ggtags-mode-map (kbd "C-c t h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c t r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c t f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c t d") 'ggtags-find-definition)
(define-key ggtags-mode-map (kbd "C-c t c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c t u") 'ggtags-update-tags)
(define-key ggtags-mode-map (kbd "C-c t g") 'ggtags-grep)
(define-key ggtags-mode-map (kbd "C-c t n") 'ggtags-navigation-mode-abort)


;; (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

;; hs-minor mode (for collapsing braces)
;; (add-hook 'c-mode-common-hook 'hs-minor-mode)
;; (global-set-key (kbd "C-c h h") 'hs-hide-block)
;; (global-set-key (kbd "C-c h s") 'hs-show-block)
;; (global-set-key (kbd "C-c h a") 'hs-hide-all)
;; (global-set-key (kbd "C-c h n") 'hs-show-all)

;; find-other-file, for switching between .h and .cpp files.
(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))


;; Rust
;; allow autocomplete in rust mode

;; Go
(require 'go-autocomplete)
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

;; Add mybin/ to the path. It contains a symlink to ~/mongo/build/clang-format so that the correct
;; version of clang format is used.
(setq exec-path (append exec-path '("/opt/mongodbtoolchain/v3/bin")))

;; clang-format buffer
(global-set-key [C-M-tab] 'clang-format-buffer)
;; special override for searching for failed to load
(global-set-key (kbd "C-M-q") (lambda() (interactive) (search-forward "failed to load" nil nil 1)))






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
 '(imenu-list-minor-mode nil)
 '(package-selected-packages
   (quote
    (string-inflection smartparens ycmd wgrep clang-format rtags logview ag flycheck-ycmd company-ycmd company project-explorer go-autocomplete helm-git-grep helm magit fiplr web-beautify flycheck json-reformat yasnippet xcscope py-autopep8 imenu-list go-mode ggtags drag-stuff auto-complete ample-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(setq-default fill-column 99)


(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
(eval-after-load 'sgml-mode
  '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))
(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))


;; git grep
(global-set-key (kbd "C-c g")
                (lambda ()
                  (interactive)
                  (progn
                    (icomplete-mode 0) ;; icomplete mode likes to be annoying during vc-git-grep
                    (call-interactively 'vc-git-grep)
                    (icomplete-mode 1))))

;; (with-eval-after-load 'helm
;;   (define-key helm-map (kbd "M-n")
;;     (lambda () (interactive) (helm-next-line 5)))
;;   (define-key helm-map (kbd "M-p")
;;     (lambda () (interactive) (helm-previous-line 5))))


;; project explore
;;(global-set-key (kbd "C-c p") 'project-explorer-open)
;;(global-set-key (kbd "C-c C-p C-s") 'project-explorer-helm)

;; C-x SPC to select rectangle
;; C-x r k to kill rectangle
;; C-x r y to yank rectangle

;; registers
;;C-x r <SPC> r
;; Record the position of point and the current buffer in register r (point-to-register). 
;; C-x r j r
;; Jump to the position and buffer saved in register r (jump-to-register).

;; To find source for a library (or package) do M-x find-library (then enter package name like: helm-git-grep or project-explorer)

;; Change default grep command
(eval-after-load "grep"
  '(progn
     (grep-apply-setting 'grep-command "grep --color -n -r src -e ")))

(set-face-attribute 'default nil :height 230)

;; Writable grep buffer:
;; run grep (vc-git-grep)
;; Enable writing: C-c C-p
;; C-c C-e to execute changes.
;; C-x C-s to save
;;

(icomplete-mode 1)

;; rtags:
;; Set up instructions here:
;; https://github.com/Andersbakken/rtags
;; Must compile with
;; cmake -DLIBCLANG_LLVM_CONFIG_EXECUTABLE=/opt/mongodbtoolchain/v2/bin/llvm-config -DCMAKE_EXPORT_COMPILE_COMMANDS=1 .
;; And from your mongo-server directory, run
;; ~/rtags/bin/rc -J .
;; Then must install latest version of rtags
(set-variable 'rtags-path "/home/ianboros/rtags/bin/")
(global-set-key (kbd "C->") 'rtags-find-symbol)
(global-set-key (kbd "C-<") 'rtags-find-references)


;; Fixing ptrace problem with gdb:
;; echo 0 > /proc/sys/kernel/yama/ptrace_scope

;; Allow emacs to display long line numbers.
(setq line-number-display-limit-width 2000000)
