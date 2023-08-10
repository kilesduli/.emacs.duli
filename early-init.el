;;; early-init.el --- Description -*- lexical-binding: t; -*-

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold (* 20 1024 1024))))

(setq native-comp-jit-compilation nil)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; (setq-default mode-line-format nil)

