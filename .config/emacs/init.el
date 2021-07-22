;; User Interface

(scroll-bar-mode -1)  ; Disable scrollbar
(tool-bar-mode -1)    ; Disable toolbar
(menu-bar-mode -1)    ; Disable menu bar
(tooltip-mode -1)     ; Disable tooltips
(set-fringe-mode 10)  ; Add breathing room
(column-number-mode)

(set-face-attribute 'default nil
		    :font "Fira Code Retina"
		    :height 101)
(set-face-attribute 'fixed-pitch nil
		    :font "Fira Code Retina"
		    :height 101)
(set-face-attribute 'variable-pitch nil
		    :font "ETBembo"
		    :height 120)


;; Package Management

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Make sure use-package is installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Make sure packages are downloaded before they are run
(require 'use-package)
(setq use-package-always-ensure t)


;; Completion

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
  :config (ivy-mode))

(use-package ivy-rich
  :init (ivy-rich-mode))

(use-package counsel
  :config (counsel-mode))


;; Keybindings

(use-package general
  :config
  (general-evil-setup)
  (general-create-definer my-leader-def
    :states 'normal
    :prefix "SPC")
  (general-create-definer my-local-leader-def
    :states 'normal
    :prefix "SPC m")
  (my-leader-def
    "b" 'counsel-switch-buffer
    "d" '(:ignore t :which-key "dotfiles")
    "de" '((lambda () (interactive)
	     (find-file "~/.config/emacs/init.el"))
	     :which-key "emacs")
    "e" '(:ignore t :which-key "eval")
    "eb" 'eval-buffer
    "ee" 'eval-last-sexp
    "f" 'find-file))

(general-def
  "C-=" `text-scale-increase
  "C--" `text-scale-decrease)

(use-package undo-tree
  :init (global-undo-tree-mode))

(use-package evil
  :init
  (setq evil-move-cursor-back nil
	evil-want-keybinding nil ;; For evil-collection
	evil-want-Y-yank-to-eol 1
	evil-undo-system 'undo-tree)
  :config
  (evil-mode)
  (general-def evil-insert-state-map "C-g" 'evil-normal-state)
  (general-def 'normal "j" 'evil-next-visual-line)
  (general-def 'normal "k" 'evil-previous-visual-line))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

;; Make ESC quit prompts
(general-def "<escape>" 'keyboard-escape-quit)

;; Escape insert mode with "jk"
(general-imap "j"
	      (general-key-dispatch 'self-insert-command
		:timeout 0.25
		"k" 'evil-normal-state))


;; Toggle line numbers format by state

(defun my-display-line-numbers ()
  (setq-local display-line-numbers 'visual
	      display-line-numbers-current-absolute t))

;(defun display-line-numbers-absolute ()
;  (setq-local display-line-numbers 1))
;(defun display-line-numbers-visual ()
;  (setq-local display-line-numbers 'visual))

(add-hook 'prog-mode-hook 'my-display-line-numbers)
;(add-hook 'evil-insert-state-entry-hook 'display-line-numbers-absolute)
;(add-hook 'evil-insert-state-exit-hook 'display-line-numbers-visual)


;; Miscellaneous

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.3))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-comand] . helpful-command)
  ([remap describe-key] . helpful-key))

;; Liked dark themes:
;;; doom-vibrant, doom-nord, doom-palenight
;; Liked light themes:
;;; doom-one-light
(use-package doom-themes
  :config
  (load-theme 'doom-one-light t)
  (doom-themes-org-config))

(use-package solaire-mode
  :config (solaire-global-mode))

(use-package doom-modeline
  :init (doom-modeline-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package projectile
  :pin melpa-stable
  :diminish projectile-mode
  :init (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package counsel-projectile
  :after projectile
  :init (counsel-projectile-mode))

(my-leader-def
  "SPC" 'projectile-find-file
  "p" '(:ignore t :which-key "projects")
  "pp" 'projectile-switch-project)

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
  (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode))

(use-package evil-cleverparens
  :hook (emacs-lisp-mode . evil-cleverparens-mode))

(use-package magit)


;; Dotfiles git bare repo
;; https://emacs.stackexchange.com/questions/30602/use-nonstandard-git-directory-with-magit

(setq dotfiles-git-dir
      (concat "--git-dir=" (expand-file-name "~/.dotfiles-git")))
(setq dotfiles-work-tree
      (concat "--work-tree=" (expand-file-name "~")))

;; Add args when used for dotfiles
(defun quark/dotfiles-magit-status ()
  (interactive)
  (add-to-list 'magit-git-global-arguments dotfiles-git-dir)
  (add-to-list 'magit-git-global-arguments dotfiles-work-tree)
  (call-interactively 'magit-status))
(my-leader-def "dg" '(quark/dotfiles-magit-status
		     :which-key "dotfiles-magit-status"))

;; Remove args otherwise
(defun quark/magit-status ()
  (interactive)
  (setq magit-git-global-arguments
	(remove dotfiles-git-dir magit-git-global-arguments))
  (setq magit-git-global-arguments
	(remove dotfiles-work-tree magit-git-global-arguments))
  (call-interactively 'magit-status))
(general-def "C-x g" 'quark/magit-status)
(general-def magit-file-mode-map "C-x g" 'quark/magit-status)
(my-leader-def "g" '(quark/magit-status
		     :which-key "magit-status"))


;; Org-mode

(defun quark/org-mode-setup ()
  (variable-pitch-mode)
  (visual-line-mode)
  (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . quark/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t
	org-startup-indented t))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "►" "◇")))

;; Prettify list bullets
;; Seems to work only sometimes?
(font-lock-add-keywords
 'org-mode
 '(("^ *\\([-]\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(set-face-attribute 'org-document-title nil :font "ETBembo" :weight 'bold :height 2.0)
(dolist (face '((org-level-1 . 1.75)
                (org-level-2 . 1.5)
                (org-level-3 . 1.25)
                (org-level-4 . 1.1)
                (org-level-5 . 1.0)
                (org-level-6 . 1.0)
                (org-level-7 . 1.0)
                (org-level-8 . 1.0)))
  (set-face-attribute (car face) nil :font "ETBembo" :weight 'medium :height (cdr face)))

;; Display certain regions in a fixed-pitch font
(set-face-attribute 'org-block nil           :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-checkbox nil        :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil            :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-formula nil         :inherit 'fixed-pitch)
(set-face-attribute 'org-meta-line nil       :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-table nil           :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil        :inherit '(shadow fixed-pitch))

(defun quark/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode))

(use-package visual-fill-column
  :hook (org-mode . quark/org-mode-visual-fill))
