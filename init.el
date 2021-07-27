;;; Package --- Summary

;;; Commentary:
;; All initialisation is done in emacs.org
;; This just loads that file

;; Before doing anything else, I need to configure melpa.org as a
;; source for packages. Also, use the orgmode.org archive for org.
(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
; local path
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Who Am I
(setq user-full-name "Greg Smolyn"
      user-mail-address "greg@smolyn.org")

;; Use-Package
;; Defer loading to speed up init
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t
	  use-package-ensure-all t
	  use-package-always-defer t)
(require 'use-package)
(require 'use-package-ensure)

;; Auto package update ??
;; (use-package auto-package-update
;;   :init
;;   (auto-package-update-maybe)
;;   (setq auto-package-update-delete-old-versions t))


(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


;; ====================================================================================
;; ****** Editor Configuration
;; *** Keep changes from 'customize' in a separate file

(setq custom-file (expand-file-name "customize.el" user-emacs-directory))
(load custom-file)

;; put saves in one place
(defvar savesdir (expand-file-name "~/.emacs.d/saves/"))
(setq backup-directory-alist
      `((".*" . ,savesdir)));,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,savesdir t)));,temporary-file-directory t)))
(setq auto-save-list-file-prefix savesdir)
(setq create-lockfiles nil)
(setq auto-revert-use-notify nil)
(global-auto-revert-mode t)

;; backups
(setq
 backup-by-copying t     ; don't clobber symlinks
 kept-new-versions 10    ; keep 10 latest versions
 kept-old-versions 0     ; don't bother with old versions
 delete-old-versions t   ; don't ask about deleting old versions
 version-control t       ; number backups
 vc-make-backup-files t) ; backup version controlled files


;; *** UI
(load-theme 'wombat t)
(powerline-center-theme)
(tool-bar-mode -1)
;(set-face-attribute 'default nil :family "Source Code Pro" :height 125 :weight 'normal)
(set-face-attribute 'default nil :family "Fira Code" :height 145 :weight 'normal)

(setq inhibit-startup-message t)

;; show column numbers
(setq column-number-mode t)
(global-display-line-numbers-mode)

;; allow for quick switching between frames
(global-set-key "\M-`" 'other-frame)

; prefer horizontal windows
(setq split-width-threshold nil)


(defalias 'yes-or-no-p 'y-or-n-p)

;; still need? TODO
(require 'smooth-scrolling)
(setq mouse-wheel-scroll-amount '(1))

;; *** Unbind the pesky sleep key
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;; enable recentf mode
(setq recentf-max-menu-items 50)
(setq recentf-max-saved-items 250)
(recentf-mode 1)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(run-at-time nil (* 5 60) 'recentf-save-list)
(setq recentf-auto-cleanup 600)

;; *** Enable overwriting selected text
(delete-selection-mode t)

;; *** Disable audible bell, use visual instead
(setq ring-bell-function 'ignore)
(setq visible-bell t)

;; *** Mac keys
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
(mac-auto-operator-composition-mode)

;;*** Show trailing white space
;; Show white space at the ends of line, to avoid embarassment when
;; comitting something. Then delete them with M-x delete-trailing-whitespace
(setq-default show-trailing-whitespace t)

;; *** Diminish
;; Use diminish so that use-package can hid modes from the mode line when
;; we ask it to.
(use-package diminish)

;; ==================== ORG MODE
(use-package org
             :pin org ;; use version from orgmode.org/elpa instead of gnu.
             :custom
             (org-src-tab-acts-natively t)
             (org-src-window-setup 'current-window) ;; edit src blocks in place, rather than a new window
             (org-hide-emphasis-markers t) ;;actually emphasise text (e.g. show as italic instead of /italic/)
             )

  (use-package org-plus-contrib
               :pin org
               :after org)


;; ========================  Markdown
;;Sometimes I need to edit markdown, so here's how to configure [[https://github.com/jrblevin/markdown-mode][markdown-mode]].
;; For README.md files, use github flavoured markdown, otherwise use normal markdown mode.

(use-package markdown-mode
             :mode (("README\\.md\\'" . gfm-mode)
	                ("\\.md\\'" . markdown-mode)
	                ("\\.markdown\\'" . markdown-mode)))

;; ==================== ** Git stuff
;;Magit is a great interface to git (although the [[https://magit.vc/manual/magit/][documentation]] is quite dense).
(use-package magit
             :bind (("C-x g" . magit-status)
	                ("C-x M-g" . magit-dispatch-popup)))



;; I like to have a visual git status in the gutter/fringe, for that I use [[https://github.com/emacsorphanage/git-gutter][git-gutter.el]]
;; diminished so it doesn't show in mode line
;;   (use-package git-gutter
;;     :diminish git-gutter-mode
;;     :init
;;     (custom-set-variables
;;      '(git-gutter:update-interval 2))
;;     :config
;;     (global-git-gutter-mode +1))

;; ========================= IVY

 (use-package ivy
    :diminish ivy-mode
    :config
    (all-the-icons-ivy-setup)
    (ivy-mode 1)
    ;; add 'recent-mode' and bookmarks to 'ivy-switch-buffer'.
    (setq ivy-use-virtual-buffers t)
    ;; number of result lines to display
    ;;(setq ivy-height 10)
    (setq ivy-count-format "(%d/%d) ")
    (setq ivy-display-style 'fancy)
    )


;; ========================= COUNSEL

(use-package counsel-projectile)
(use-package counsel
             :diminish counsel-mode
             :config
             (counsel-mode 1))

;; ========================= SWIPER

(use-package swiper
             :commands (swiper swiper-all)
             :bind ("C-S-s" . 'swiper))



;; ========================= PROJECTILE

(use-package projectile
             :demand
             :bind (:map projectile-mode-map
	                     ("C-c p" . projectile-command-map))
             :init
             (setq projectile-completion-system 'ivy)
             (setq projectile-enable-caching t)
             :config
             (add-to-list 'projectile-globally-ignored-files "node-modules")
             (projectile-mode))

(use-package counsel-projectile
             :demand
             :config
             (counsel-projectile-mode))

;; ================================ ** Code/Text Completion (company-mode)
;; Got to have those sweet code-completion popups, courtesy of [[https://company-mode.github.io/][company-mode]].
(use-package company
             :diminish
             :init
             (global-company-mode))



;; ================================ ** Color in compilation buffer
;;
;; (defun colorize-compilation-buffer ()
;;   (when (derived-mode-p 'compilation-mode)
;; 	(ansi-color-process-output nil)
;; 	(setq-local comint-last-output-start (point-marker))))
;; (use-package ansi-color
;;   :config
;;   (add-hook 'compilation-filter-hook
;; 			#'colorize-compilation-buffer))

(require 'xterm-color)
(setq compilation-environment '("TERM=xterm-256color"))

;; (defun my/advice-compilation-filter (f proc string)
;;   (funcall f proc (xterm-color-filter string)))
(defun my/advice-compilation-filter (f proc string)
  (funcall f proc (if (string-prefix-p "*compilation" (buffer-name (process-buffer proc)))
                      (xterm-color-filter string) string)))

(advice-add 'compilation-filter :around #'my/advice-compilation-filter)

(defconst typescript-tsc-pretty-error-regexp
  (concat
   "^[[:blank:]]*"
   "\\([^(\r\n)]+\\):\\([0-9]+\\):\\([0-9]+\\) - [[:blank:]]*"
   "error [[:alnum:]]+: [^\r\n]+$")
  "Regexp to match errors generated by tsc.")

(dolist
    (regexp
     `( (typescript-tsc-pretty
        ,typescript-tsc-pretty-error-regexp
        1 2 3 2)))

  (add-to-list 'compilation-error-regexp-alist-alist regexp)
  (add-to-list 'compilation-error-regexp-alist (car regexp)))


(require 'compile-eslint)
(push 'eslint compilation-error-regexp-alist)


(setq shell-file-name "zsh")
(setq shell-command-switch "-ic")


;; ============================= ** Development languages
;;

(setq-default indent-tabs-mode nil)

;; I'm going to try lsp-mode again, for languages it supports.
;; Here's the core lsp-configuration:
(setq lsp-keymap-prefix "C-l")



(use-package lsp-mode
             :hook (
	                ;; bind lsp to the development modes I'm interested in.
	                (web-mode . lsp-deferred)
	   (lsp-mode . lsp-enable-which-key-integration))
             :init
             (setq lsp-enable-completion-at-point t)
    (setq lsp-enable-indentation t)
    (setq lsp-enable-on-type-formatting t)
    :commands lsp lsp-deferred)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package which-key
             :config
             (which-key-mode))

;; lsp-mode perf
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-completion-provider :capf)
(setq gc-cons-threshold 100000000)


;; web-mode
  (use-package web-mode
    :ensure t
    :mode (("\\.js\\'" . web-mode)
	   ("\\.jsx\\'" . web-mode)
	   ("\\.ts\\'" . web-mode)
	   ("\\.tsx\\'" . web-mode)
	   ("\\.html\\'" . web-mode)
	   ("\\.vue\\'" . web-mode)
	   ("\\.json\\'" . web-mode))
    :commands web-mode
    :config
    (setq company-tooltip-align-annotations t)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-enable-part-face t)
    (setq web-mode-content-types-alist
	  '(("jsx" . "\\.js[x]?\\'")))
    )


; use whitespace butler to remove extra whitespace
(use-package ws-butler
             :diminish
             :init
             (ws-butler-global-mode 1))


;; searching
;; keep multi-occur
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)
(global-set-key (kbd "C-c o") 'multi-occur)

;; ag
(setq ag-reuse-buffers 't)

;; fzf
(setenv "FZF_DEFAULT_COMMAND" "fd --type f")

(server-start)
