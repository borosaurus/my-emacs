(require 'package)
; add MELPA to repository list
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
; initialize package.elx
(package-initialize)

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
;;(ac-config-default)

; M-x package-install yasnippet
; yasnippet (gives us templates for loops and functions and stuff)
(require 'yasnippet)
(yas-global-mode 1)

; package-install auto-complete-c-headers
(defun my:ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
)
; now let's call this function from c/c++ hooks
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)


; package-install iedit
; (Weird bug with iedit)
(define-key global-map (kbd "C-c ;") 'iedit-mode)

; package-install ecb
(require 'ecb)


(setq default-frame-alist
      '((top . 250) (left . 400)
        (width . 80) (height . 600)
        (cursor-color . "white")
        (cursor-type . box)
        (foreground-color . "blue")
        (background-color . "white")
        (font . "-*-Courier-normal-r-*-*-14-*-*-*-c-*-iso8859-1")))

;; Use ibuffer instead of regular buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)


(setq initial-frame-alist '((top . 50) (left . 30)))

;; Pyret
(ignore-errors
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/"))
  (require 'pyret)
  (add-to-list 'auto-mode-alist '("\\.arr$" . pyret-mode))
  (add-to-list 'file-coding-system-alist '("\\.arr\\'" . utf-8)))

;; C
(setq c-default-style "bsd"
          c-basic-offset 4)

;; Use spaces insteads of tabs
(setq tab-width 4)
(setq-default indent-tabs-mode nil)


(setq column-number-mode t)
(add-to-list 'default-frame-alist '(height . 55))
(add-to-list 'default-frame-alist '(width . 80))

;; cedet setup
; turn on Semantic
(semantic-mode 1)
; let's define a function which adds semantic as a suggestion backend to auto complete
; and hook this function to c-mode-common-hook
(defun my:add-semantic-to-autocomplete() 
  (add-to-list 'ac-sources 'ac-source-semantic)
)
(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)
(global-semantic-idle-scheduler-mode 1)


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
(global-set-key (kbd "<M-left>") 'ecb-goto-window-methods)
(global-set-key (kbd "<M-right>") 'ecb-goto-window-edit1)

(global-set-key (kbd "<C-left>") 'windmove-left)
(global-set-key (kbd "<C-right>") 'windmove-right)
(global-set-key (kbd "<C-up>") 'windmove-up)
(global-set-key (kbd "<C-down>") 'windmove-down)


