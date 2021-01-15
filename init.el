;;; package --- Summary
(setq inhibit-startup-message t)
;;; Code:
(scroll-bar-mode -1)    ; Disable visible scrollbar
(tool-bar-mode -1)      ; Disable the toolbar
(tooltip-mode -1)       ; Disable tooltips
(set-fringe-mode 10)    ; Give some breathing room

(menu-bar-mode -1)      ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(set-face-attribute 'default nil :font "Iosevka" :height 200 :weight 'Thin)

;; 启动全屏 
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Auto complete the brackets
(setq electric-pair-preserve-balance nil)

;; Make ESC quit prompts
(global-set-key (kbd "<esacpe>") 'keyboard-escape-quit)

;; Set up the Exec Path
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/Users/liny/Lib/go/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/Users/liny/Lib/go/bin")))

(bind-key "s-w" #'kill-this-buffer)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
			 ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-ook
		shell-mode-hook
		treeemacs-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :custom
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-style "rounded")
  (centaur-tabs-height 34)
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "●")
  (centaur-tabs-buffer-groups-function #'centaur-tabs-projectile-buffer-groups)

  :bind
  (("s-{" . #'centaur-tabs-backward)
   ("s-}" . #'centaur-tabs-forward)))

(use-package dired-sidebar
  :bind (("s-b" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar))

(use-package command-log-mode)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)	 
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't stat search with ^

(global-set-key (kbd "C-M-b") 'counsel-switch-buffer)

(define-key emacs-lisp-mode-map (kbd "C-x M-t") 'counsel-load-theme)

(use-package vscode-icon
  :ensure t
  :commands (vscode-icon-for-file))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 8)))

(use-package doom-themes
	     :init (load-theme 'doom-laserwave t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;(use-package general
;  :config
;  (general-create-definer rune/leader-keys
;    :keymaps '(normal insert visual emacs)
;    :prefix "SPC"
;    :global-prefix "C-SPC")
;
;  (rune/leader-keys
;    "t"  '(:ignore t :which-key "toggle")
;    "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (define-key evil-normal-state-map (kbd "C-r") 'evil-scroll-line-up)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'message-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

;;(rune/leader-keys
;;  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode 1)
  (setq projectile-sort-order 'recently-active)
  (setq projectile-enable-caching t)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Project/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
	:after (ivy counsel))

;; Language Servers
(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  ;;(setq lsp-clients-php-iph-server-command '("/usr/local/bin/intelephense"))
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-treemacs
  :after lsp)

;; Language php
(use-package php-mode
  :mode ("\\.php\\'" . php-mode)
  :hook (php-mode . lsp-deferred))

(use-package go-mode
  :mode "\\.go\\'"
  :hook (go-mode . lsp-deferred)
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
	      ("RET" . company-complete-selection))
        (:map lsp-mode-map
	      ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :custom
  (lsp-ui-doc-position 'right))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-shell "/bin/zsh")
  (setq vterm-max-scrollback 10000))

;; (defun dw/org-mode-setup ()
;;   (org-indent-mode)
;;   (variable-pitch-mode 1)
;;   (auto-file-mode 0)
;;   (visual-line-mode 0)
;;   (setq evil-auto-indent nil))

(use-package org
  ;; :hook (org-mode . dw/org-mode-setup)
  :config
  (setq org-ellipsis " ◆"))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(font-lock-add-keywords 'org-mode
			'(("^ *\\([-]\\)"
			   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "●"))))))

(dolist (face '((org-level-1 . 1.2)
		(org-level-2 . 1.1)
		(org-level-3 . 1.05)
		(org-level-4 . 1.0)
		(org-level-5 . 1.1)
		(org-level-6 . 1.1)
		(org-level-7 . 1.1)
		(org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(centaur-tabs-buffer-groups-function #'centaur-tabs-projectile-buffer-groups t nil "Customized with use-package centaur-tabs")
 '(centaur-tabs-gray-out-icons 'buffer nil nil "Customized with use-package centaur-tabs")
 '(centaur-tabs-height 34 nil nil "Customized with use-package centaur-tabs")
 '(centaur-tabs-modified-marker "●" nil nil "Customized with use-package centaur-tabs")
 '(centaur-tabs-set-icons t nil nil "Customized with use-package centaur-tabs")
 '(centaur-tabs-set-modified-marker t nil nil "Customized with use-package centaur-tabs")
 '(centaur-tabs-style "rounded" nil nil "Customized with use-package centaur-tabs")
 '(company-idle-delay 0.0 nil nil "Customized with use-package company")
 '(company-minimum-prefix-length 1 nil nil "Customized with use-package company")
 '(company-quickhelp-color-background "#b0b0b0")
 '(company-quickhelp-color-foreground "#232333")
 '(counsel-describe-function-function #'helpful-callable nil nil "Customized with use-package helpful")
 '(counsel-describe-variable-function #'helpful-variable nil nil "Customized with use-package helpful")
 '(custom-safe-themes
   '("74ba9ed7161a26bfe04580279b8cad163c00b802f54c574bfa5d924b99daa4b9" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" default))
 '(doom-modeline-height 10 nil nil "Customized with use-package doom-modeline")
 '(lsp-ui-doc-position 'right nil nil "Customized with use-package company-box")
 '(nrepl-message-colors
   '("#336c6c" "#205070" "#0f2050" "#806080" "#401440" "#6c1f1c" "#6b400c" "#23733c"))
 '(org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●") nil nil "Customized with use-package org-bullets")
 '(package-selected-packages
   '(dired-sidebar anti-zenburn-theme centaur-tabs php-mode which-key vterm vscode-icon use-package spacemacs-theme rainbow-delimiters org-bullets lsp-ui lsp-treemacs lsp-ivy ivy-rich helpful go-mode general evil-nerd-commenter evil-magit evil-collection eterm-256color dw doom-themes doom-modeline counsel-projectile company-box command-log-mode))
 '(projectile-completion-system 'ivy nil nil "Customized with use-package projectile"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
