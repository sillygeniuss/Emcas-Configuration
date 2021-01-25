;;; package --- Summary
(setq inhibit-startup-message t)
;;; Code:
(scroll-bar-mode -1)     ; Disable visible scrollbar
(tool-bar-mode -1)       ; Disable the toolbar
(tooltip-mode -1)        ; Disable tooltips
(set-fringe-mode 10)     ; Give some breathing room
(menu-bar-mode -1)       ; Disable the menu bar
(show-paren-mode 1)      ; 成对符号匹配模式
(global-hl-line-mode 1)  ; 高亮光标当前行
(read-only-mode -1)      ; 关闭Buffer只读模式
(column-number-mode)
(global-display-line-numbers-mode t)

; 自定义配置
(custom-set-variables
 '(spacemacs-theme-custom-colors '((bg1 . "#212026") (bg2 . "#292B2E")))
 '(spacemacs-theme-keyword-italic t))

;; Set up the HTTP proxy
(setq url-gateway-method 'socks)
(setq socks-server '("SS Server" "127.0.0.1" 1086 5))

;; Set up the visible bell
(setq visible-bell t)

;; Disable files backup function
(setq make-backup-files nil)
(setq create-lockfiles nil)

(set-face-attribute 'default nil :font "Cascadia Code" :height 160 :weight 'Thin)

;; 启动全屏 
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Split the window vertically
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; Auto complete the brackets
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode 1)

;; Make ESC quit prompts
(global-set-key (kbd "<esacpe>") 'keyboard-escape-quit)

;; Set up the Exec Path
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/Users/liny/Lib/go/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/Users/liny/Lib/go/bin")))

(bind-key "s-w" #'kill-this-buffer)

;; Initialize package sources
(require 'package)

;; Package sources configuration
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Packages configuration
(use-package diminish)

(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer dw/leader-key-def
    :keymaps '(normal emacs)
    :prefix "SPC"
    :global-prefix "C-c SPC")

  (general-create-definer dw/ctrl-c-keys
    :prefix "C-c"))

(use-package avy
  :commands (avy-goto-char avy-goto-word-0 ayv-goto-line))
(dw/leader-key-def
  "j" '(:ignore t :which-key "jump")
  "jj" '(avy-goto-char :which-key "jump to char")
  "jw" '(avy-goto-word-0 :which-key "jump to word")
  "jl" '(avy-goto-line :which-key "jump to line"))

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

(use-package vscode-icon
  :ensure t
  :commands (vscode-icon-for-file))

(use-package dired-sidebar
  :bind (("s-b" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(use-package command-log-mode)

(use-package ivy
  :diminish ivy-mode
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
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  (ivy-use-selectable-prompt t)
  :config
  (ivy-mode)
  (setq enable-recursive-minibuffers t))

(use-package ivy-rich
  :after ivy
  :demand
  :custom
  (ivy-virtual-abbreviate 'full
			  ivy-rich-switch-buffer-align-virtual-buffer t
			  ivy-rich-path-style 'abbrev)
  :config
  (setq ivy-rich-parse-remote-buffer nil)
  (setq ivy-rich-parse-remote-file-path nil)
  ;(ivy-set-display-transformer 'ivy-switch-buffer
  ;                             'ivy-rich-switch-buffer-transformer)
  (ivy-rich-mode 1))

(use-package ivy-xref
  :init
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package counsel
  :after ivy
  :demand
  :diminish counsel-mode
  :init
  (setq ivy-use-virtual-buffers t
	ivy-re-builders-alist
	'((counsel-rg . ivy--regex-plus)
	  (swiper . ivy--regex-plus)
	  (t . ivy--regex-plus)))
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (add-to-list 'ivy-ignore-buffers "\\`\\*remind-bindings\\*")
  (counsel-mode 1)
  ;;<<do.minimal/counsel-ripgrep>>
  (setq ivy-initial-inputs-alist
	'((counsel-minor . "^+")
	  (counsel-package . "^+")
	  (counsel-org-capture . "")
	  (counsel-M-x . "")
	  (counsel-describe-function . "")
	  (counsel-describe-variable . "")
	  (org-refile . "^")
	  (org-agenda-refile . "^")
	  (org-capture-refile . "^")
	  (Man-completion-table . "^")
	  (woman . "^"))))

(global-set-key (kbd "C-M-b") 'counsel-switch-buffer)
(global-set-key (kbd "C-c f") 'counsel-rg)

(define-key emacs-lisp-mode-map (kbd "C-x M-t") 'counsel-load-theme)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 8)))

(use-package doom-themes
	     :init (load-theme 'spacemacs-dark t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package magit
  :config
  (global-set-key (kbd "C-c g") 'magit-status)
  :custom
  (magit-git-executable "/usr/local/bin/git")
  :init
  (use-package with-editor :ensure t)
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-window))
  (defadvice magit-quit-window (after magit-restore-screen activate)
    (jump-to-register :magit-fullscreen))
  :config
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent))

(dw/leader-key-def
  "gs" 'magit-status
  "gd" 'magit-diff-unstaged
  "gc" 'magit-branch-or-checkout
  "gb" 'magit-branch
  "gP" 'magit-push-current
  "gp" 'magit-pull-branch
  "gr" 'magit-rebase
  "gm" 'magit-merge
  "gB" 'magit-blame)

(use-package nyan-mode
  :config
  (nyan-mode 1))

(use-package undo-fu)

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
  (define-key evil-normal-state-map (kbd "u") 'undo-fu-only-undo)
  (define-key evil-normal-state-map (kbd "\C-r") 'undo-fu-only-redo)

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

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode 1)
  (setq projectile-sort-order 'recently-active)
  (setq projectile-enable-caching t)
  :demand t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  ((projectile-completion-system 'ivy))
  :init
  (when (file-directory-p "~/Project/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
	:after (projectile))

(dw/leader-key-def
  "pr" 'counsel-projectile-rg
  "pf" 'counsel-projectile-find-file
  "ps" 'counsel-projectile-switch-project)

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

(use-package flycheck
  :config
  (global-flycheck-mode 1))

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

(use-package ivy-posframe
  :after ivy
  :diminish ivy-posframe-mode
  :custom-face
  (ivy-posframe-border ((t (:background "#FFFAFA"))))
  :custom
  (ivy-posframe-width 115)
  (ivy-posframe-min-width 115)
  (ivy-posframe-height 10)
  (ivy-posframe-min-height 10)
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-border-width 1)
  (setq ivy-posframe-parameters '((left-fringe . 8)
				  (right-fringe . 8)))
  (setq ivy-posframe-hide-minibuffer t)
  (ivy-posframe-mode 1))

(dolist (face '((org-level-1 . 1.2)
		(org-level-2 . 1.1)
		(org-level-3 . 1.05)
		(org-level-4 . 1.0)
		(org-level-5 . 1.1)
		(org-level-6 . 1.1)
		(org-level-7 . 1.1)
		(org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))
