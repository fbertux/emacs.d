;;; early-init.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Early settings
(setq gc-cons-threshold (* 100 1024 1024))
(menu-bar-mode -1)
(tool-bar-mode -1)
(if (display-graphic-p)
    (scroll-bar-mode -1))
(unless (display-graphic-p)
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq package-enable-at-startup nil)
(setq site-run-file nil)
