;; User Interface

(scroll-bar-mode -1)  ; Disable scrollbar
(tool-bar-mode -1)    ; Disable toolbar
(menu-bar-mode -1)    ; Disable menu bar
(tooltip-mode -1)     ; Disable tooltips
(set-fringe-mode 10)  ; Add breathing room

(set-face-attribute 'default nil :font "Fira Code Retina" :height 100)
(load-theme 'leuven)


;; Keys

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


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


;; Packages

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

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
