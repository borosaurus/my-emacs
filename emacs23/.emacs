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


