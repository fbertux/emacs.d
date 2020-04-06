;;; init.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Early settings
(menu-bar-mode -1)
(setq gc-cons-threshold 67108864)
(setq large-file-warning-threshold 100000000)
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
    (yaml-mode ws-butler use-package undo-tree systemd smex smartparens sane-term rainbow-delimiters projectile pkgbuild-mode pdf-tools ob-async mmm-mode latex-preview-pane json-mode htmlize go-guru go-eldoc gnu-elpa-keyring-update git-timemachine git-gutter forge flycheck flx-ido expand-region dts-mode doom-themes dockerfile-mode diminish company browse-kill-ring auctex aggressive-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
