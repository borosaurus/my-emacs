(require 'package)
; add MELPA to repository list
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

; initialize package.elx
(package-initialize)

;; disable toolbar
(tool-bar-mode -1)
(setq use-dialog-box nil)

(load-theme 'ample t t)
(enable-theme 'ample)

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (load-theme 'ample t t)
              (enable-theme 'ample))))

;; Use ibuffer rather than regular buffer list.
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Treat .h files as C++ rather than C.
(add-to-list 'auto-mode-alist '("\\.[h]\\'" . c++-mode))

;; Use spaces insteads of tabs
(setq tab-width 4)
(setq-default indent-tabs-mode nil)
;; C
(setq c-default-style "bsd"
      c-basic-offset 4)

;; Add stuff to the path (clang format, clangd etc).
(setq exec-path (append exec-path '("/opt/mongodbtoolchain/v3/bin")))

;; 100 column/char limit.
(setq whitespace-line-column 100)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)
(setq-default fill-column 99)

;; ido mode (better for finding files etc).
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Better mini-buffer completions.
(icomplete-mode 1)

;; Allow emacs to display long line numbers.
(setq line-number-display-limit-width 2000000)

;; lsp-mode. For this to work clangd *must* be in the exec path. Above we append exec-path
;; to ensure the full toolchain is in the path.
(which-key-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)


(setq company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)

;; drag-stuff mode.
(drag-stuff-mode t)
(drag-stuff-global-mode 1)
(global-set-key (kbd "C-M-n") 'drag-stuff-down)
(global-set-key (kbd "C-M-p") 'drag-stuff-up)

;; Key binding to switch between .h and .cpp files.
(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))


(define-key global-map (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "M-n")
    (lambda () (interactive) (next-line 5)))
(global-set-key (kbd "M-p")
    (lambda () (interactive) (previous-line 5)))

(global-set-key (kbd "<C-left>") 'windmove-left)
(global-set-key (kbd "<C-right>") 'windmove-right)
(global-set-key (kbd "<C-up>") 'windmove-up)
(global-set-key (kbd "<C-down>") 'windmove-down)

(global-set-key (kbd "C-c g")
                (lambda ()
                  (interactive)
                  (progn
                    (icomplete-mode 0) ;; icomplete mode likes to be annoying during vc-git-grep
                    (call-interactively 'vc-git-grep)
                    (icomplete-mode 1))))

(global-set-key [C-M-tab] 'clang-format-buffer)

;; M-. = find definition (will work with lsp mode)
(global-set-key (kbd "C-c r") 'xref-find-references)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (clang-format yasnippet spinner lsp-mode company magit flycheck wgrep smartparens exec-path-from-shell drag-stuff ample-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
