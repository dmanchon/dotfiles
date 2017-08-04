;; Bootstrap
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

(require 'package)
(package-initialize)

(require 'org)
(org-babel-load-file "~/.emacs.d/config.org")
