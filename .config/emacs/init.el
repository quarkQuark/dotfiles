;; User Interface

(scroll-bar-mode -1)  ; Disable scrollbar
(tool-bar-mode -1)    ; Disable toolbar
(menu-bar-mode -1)    ; Disable menu bar
(tooltip-mode -1)     ; Disable tooltips
(set-fringe-mode 10)  ; Add breathing room
(column-number-mode)

(set-face-attribute 'default nil :font "Fira Code Retina" :height 101)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Package Management

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
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
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :config
  (counsel-mode 1))


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
    "e" '(:ignore t :which-key "eval")
    "eb" 'eval-buffer
    "ee" 'eval-last-sexp
    "f" 'find-file
    "d" '(:ignore t :which-key "dotfiles")
    "de" '(lambda () (interactive)
	     (find-file "~/.config/emacs/init.el"))
	     :which-key "emacs"))

(use-package evil
  :init
  (setq evil-want-keybinding nil) ;; Do not load extra evil keybindings for other modes
  (setq evil-want-Y-yank-to-eol 1)
  :config
  (evil-mode 1)
  (general-def evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (general-def 'motion "j" 'evil-next-visual-line)
  (general-def 'motion "k" 'evil-previous-visual-line))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Make ESC quit prompts
(general-def (kbd "<escape>") 'keyboard-escape-quit)

;; Escape insert mode with "jk"
(general-imap "j"
	      (general-key-dispatch 'self-insert-command
		:timeout 0.25
		"k" 'evil-normal-state))


;; Toggle line numbers format by state

(setq-default display-line-numbers 'visual
              display-line-numbers-current-absolute t)

(add-hook 'evil-insert-state-entry-hook
	  '(lambda () (interactive)
	     (setq-local display-line-numbers 'visual)))

(add-hook 'evil-insert-state-exit-hook
	  '(lambda () (interactive)
	     (setq-local display-line-numbers t)))


;; Miscellaneous

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
  :config (solaire-global-mode 1))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map))
