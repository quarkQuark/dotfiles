;;; Minimal emacs configuration to be loaded when publishing literate config files.

;; Initialise package manager, in case of old emacs version.
(require 'package)
(package-initialize)
(package-refresh-contents)
(package-install htmlize)

;; Set up org-mode.
(require 'org)
(require 'htmlize)
(setq org-html-htmlize-output-type 'css)
