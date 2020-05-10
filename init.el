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
    (qt-pro-mode go-guru go-eldoc go-mode latex-preview-pane auctex pdf-tools mmm-mode sane-term ox-hugo ob-async htmlize smex flx-ido aggressive-indent yaml-mode systemd pkgbuild-mode json-mode dts-mode dockerfile-mode forge git-gutter git-timemachine magit flycheck company undo-tree ws-butler smartparens rainbow-delimiters expand-region browse-kill-ring projectile gnu-elpa-keyring-update diminish zenburn-theme use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
