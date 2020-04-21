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
