;;; early-init.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Early settings

;; Speed-up start
(setq gc-cons-threshold (* 100 1024 1024))

(setq load-prefer-newer t)
(setq site-run-file nil)

(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)

;; Silence startup message
(setq inhibit-splash-screen t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message (user-login-name))

(setq package-enable-at-startup nil)

;; Default frame configuration
(menu-bar-mode -1)
(tool-bar-mode -1)

(if (display-graphic-p)
    (scroll-bar-mode -1))

(setq default-frame-alist '((fullscreen . maximized)
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)
                            (tool-bar-lines . nil)
                            ;; Setting the face in here prevents flashes of
                            ;; color as the theme gets activated
                            (background-color . "#383838")
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)))
