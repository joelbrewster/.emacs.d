;;; early-init.el --- -*- lexical-binding: t -*-

;; DeferGC
(setq gc-cons-threshold (* 50 1000 1000))
;; -DeferGC

;; UnsetPES
(setq package-enable-at-startup nil)
;; -UnsetPES

;; UnsetFNHA
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
;; -UnsetFNHA

;; UnsetSRF
(setq site-run-file nil)
;; -UnsetSRF

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'.
(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(set-face-attribute 'variable-pitch nil :family "PragmataPro Mono" :height 140 :weight 'normal)
(set-face-attribute 'fixed-pitch nil :family "PragmataPro Mono" :height 140 :weight 'normal)
(set-face-attribute 'default nil  :family "PragmataPro Mono" :height 140 :weight 'normal)
;; (set-face-attribute 'variable-pitch nil :family "SF Mono" :height 130 :weight 'normal)
;; (set-face-attribute 'fixed-pitch nil :family "SF Mono" :height 130 :weight 'normal)
;; (set-face-attribute 'default nil  :family "SF Mono" :height 130 :weight 'normal)

;; More spacing around lines
(setq-default line-spacing 4)

;; Speed up boot by limiting resize
(setq frame-inhibit-implied-resize t)

;; Resize off pixels
(setq frame-resize-pixelwise t)

;; Ignore X resources
(advice-add #'x-apply-session-resources :override #'ignore)

;; Prevent unwanted runtime builds
(setq comp-deferred-compilation nil)

;; Make startup look cleaner
(setq-default inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(setq-default visual-line-fringe-indicators nil)

;; Remove the annoying cl is deprecated warning
(setq byte-compile-warnings '(cl-functions))

(provide 'early-init)
