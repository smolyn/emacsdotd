;;; early-init.el --- early bird  -*- no-byte-compile: t -*-
(setq load-prefer-newer t)
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)
;;; early-init.el ends herepacked
