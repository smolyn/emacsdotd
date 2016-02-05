; gsmolyn's init.
; yay2

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ))
(setq package-enable-at-startup nil)
(package-initialize)

; local path
(add-to-list 'load-path "~/.emacs.d/lisp/")

;(setq debug-on-error t)

(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append '("/usr/local/bin") exec-path))

;; tramp

(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(setq tramp-default-method "ssh")

;; programming
(global-prettify-symbols-mode +1)

;; c-mode

(setq c-basic-offset 4)
(setq tab-width 4)
(setq indent-tabs-mode nil)

(setq-default indent-tabs-mode nil)

;; files

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

(defvar savesdir (expand-file-name "~/.emacs.d/saves/"))

(setq backup-directory-alist
      `((".*" . ,savesdir)));,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,savesdir t)));,temporary-file-directory t)))
(setq auto-save-list-file-prefix savesdir)
(setq backup-by-copying t)
(setq create-lockfiles nil)
(setq auto-revert-use-notify nil)
(global-auto-revert-mode t)

;; (setq backup-directory-alist `(("." . "~/.saves")))
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; searching
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)
(global-set-key (kbd "C-c o") 'multi-occur)

;; file types

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.cmp\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.theme\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.tokens\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.app\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.auradoc\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.intf\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.evt\\'" . xml-mode))

;; projectile/helm
(require 'helm-config)
(projectile-global-mode)
(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq helm-quick-update t
     helm-idle-delay 0.01
    helm-input-idle-delay 0.01)
(setq helm-ff-file-name-history-use-recentf t)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)
(setq projectile-tags-command "/usr/local/bin/ctags -Re -f %s %s")
(setq projectile-globally-ignored-file-extensions '(".o" ".d" ".pyc" ".class" ))

(setq ag-reuse-buffers 't)

;; flycheck
(require 'flycheck)
(setq flycheck-jshintrc "~/.emacs.d/.jshintrc")
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))

;; org-mode
(require 'org)
(setq org-log-done t)

(require 'xunitjs)
;;
;; jsx/react
;;
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))
(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."

  :command ("jsxhint" source)
  :error-patterns
  ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes (web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "jsx")
              ;; enable flycheck
              (flycheck-select-checker 'jsxhint-checker)
              (flycheck-mode))))
(defun my-web-mode-hook ()
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-enable-auto-closing nil))
(add-hook 'web-mode-hook 'my-web-mode-hook)

;; visual/UI

(load-theme 'wombat t)
(set-face-attribute 'default nil :family "Source Code Pro" :height 125 :weight 'normal)
(setq inhibit-startup-message t)
(setq column-number-mode t)
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
(global-set-key "\M-`" 'other-frame)

(setq split-width-threshold nil) ; prefer horizontal windows

(powerline-center-theme)

(defalias 'yes-or-no-p 'y-or-n-p)

;(require 'flx-ido)
;(ido-mode 1)
;(ido-everywhere 1)
;(flx-ido-mode 1)
;(setq flx-ido-threshold 10000)

(require 'smooth-scrolling)
(setq mouse-wheel-scroll-amount '(1))
;(setq mouse-wheel-progressive-speed nil)


;(require 'minimap)
;(setq gc-cons-threshold 20000000)
(setq gc-cons-threshold 100000000)




; FUNCTIONS
;
;
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (when (and (buffer-file-name buffer)
                 (not (buffer-modified-p buffer)))
        (set-buffer buffer)
        (revert-buffer t t t))
      (setq list (cdr list))
      (setq buffer (car list))))
  (message "Refreshed open files"))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ido-enable-flex-matching t)
 '(ido-ubiquitous-mode t)
 '(ido-use-faces nil)
 '(linum-delay t)
 '(nxml-child-indent 4)
 '(projectile-project-root-files
   (quote
    ("rebar.config" "project.clj" "SConstruct" "build.sbt" "build.gradle" "Gemfile" "requirements.txt" "tox.ini" "package.json" "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml" "mix.exs")))
 '(tab-stop-list nil)
 '(tab-width 4)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
