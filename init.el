;;; init.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Early settings
(setq gc-cons-threshold (* 100 1024 1024))
(menu-bar-mode -1)
(setq package-enable-at-startup nil)
(setq site-run-file nil)
(unless (display-graphic-p)
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(org-babel-load-file "~/.emacs.d/settings.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ox-gfm zenburn-theme yaml-mode ws-butler use-package undo-tree systemd smex smartparens smart-mode-line sane-term rainbow-delimiters qt-pro-mode projectile pkgbuild-mode pdf-tools ox-hugo org-noter org-journal ob-async mmm-mode latex-preview-pane json-mode htmlize go-guru go-eldoc gnu-elpa-keyring-update git-timemachine git-gutter forge flycheck flx-ido expand-region dts-mode dockerfile-mode diminish company browse-kill-ring auctex aggressive-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
