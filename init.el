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

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
;; (when (memq window-system '(mac ns))
(exec-path-from-shell-initialize)

;; (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
;; (setq exec-path (append '("/usr/local/bin") exec-path))

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


;; octave
;; (require 'ac-octave)
;; (defun ac-octave-mode-setup ()
;;   (setq ac-sources '(ac-complete-octave)))
;; (add-hook 'octave-mode-hook
;;           '(lambda () (ac-octave-mode-setup)))


;; projectile
; change mode line for perf reasons (broken)
; see https://github.com/bbatsov/projectile/issues/1183
;; (setq projectile-mode-line
;;          '(:eval (format " Projectile[%s]"
;;                         (projectile-project-name))))
;; (setq projectile-indexing-method 'alien)
;; (setq projectile-enable-caching t)
;; (setq projectile-completion-system 'ivy)
;; (setq projectile-tags-command "/usr/local/bin/ctags -Re -f %s %s")
;; (setq projectile-globally-ignored-file-extensions '(".o" ".d" ".pyc" ".class" ))
;; (projectile-global-mode)

;; projectile/helm
;; (require 'helm-config)
;; (helm-projectile-on)
;; (setq helm-quick-update t
;;      helm-idle-delay 0.01
;;     helm-input-idle-delay 0.01)
;; (setq helm-ff-file-name-history-use-recentf t)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (helm-mode 1)

;; ivy
(all-the-icons-ivy-setup)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-re-builders-alist
      '((t . ivy--regex-ignore-order)))
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
(setq ivy-display-style 'fancy)
;;advise swiper to recenter on exit
(defun bjm-swiper-recenter (&rest args)
  "recenter display after swiper"
  (recenter)
  )
(advice-add 'swiper :after #'bjm-swiper-recenter)

;; ag
(setq ag-reuse-buffers 't)


;; ==============  flycheck

(require 'flycheck)
(setq flycheck-jshintrc "~/.emacs.d/.jshintrc")
(setq flycheck-eslintrc "~/.emacs.d/.eslintrc")
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))
(flycheck-add-mode 'javascript-eslint 'js2-mode)
(flycheck-add-mode 'javascript-eslint 'js-mode)

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; =============    org-mode
(require 'org)
(setq org-log-done t)

(require 'xunitjs)

;;
;; jsx/react
;;
;; (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
;; (defadvice web-mode-highlight-part (around tweak-jsx activate)
;;   (if (equal web-mode-content-type "jsx")
;;       (let ((web-mode-enable-part-face nil))
;;         ad-do-it)
;;     ad-do-it))
;; (flycheck-define-checker jsxhint-checker
;;   "A JSX syntax and style checker based on JSXHint."

;;   :command ("jsxhint" source)
;;   :error-patterns
;;   ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
;;   :modes (web-mode))
;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (when (equal web-mode-content-type "jsx")
;;               ;; enable flycheck
;;               (flycheck-select-checker 'jsxhint-checker)
;;               (flycheck-mode))))
;; (defun my-web-mode-hook ()
;;   (setq web-mode-enable-auto-quoting nil)
;;   (setq web-mode-enable-auto-closing nil))
;; (add-hook 'web-mode-hook 'my-web-mode-hook)

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
(global-display-line-numbers-mode)

(setq split-width-threshold nil) ; prefer horizontal windows

(global-display-line-numbers-mode)

(powerline-center-theme)

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'smooth-scrolling)
(setq mouse-wheel-scroll-amount '(1))


(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;;(setq gc-cons-threshold 10000000)

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

; fzf
;(use-package "fzf" :init (setenv "FZF_DEFAULT_COMMAND" "fd --type f"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(counsel-async-filter-update-time 1000)
 '(helm-completing-read-handlers-alist
   (quote
    ((describe-function . helm-completing-read-symbols)
     (describe-variable . helm-completing-read-symbols)
     (describe-symbol . helm-completing-read-symbols)
     (debug-on-entry . helm-completing-read-symbols)
     (find-function . helm-completing-read-symbols)
     (disassemble . helm-completing-read-symbols)
     (trace-function . helm-completing-read-symbols)
     (trace-function-foreground . helm-completing-read-symbols)
     (trace-function-background . helm-completing-read-symbols)
     (find-tag . helm-completing-read-with-cands-in-buffer)
     (org-capture . helm-org-completing-read-tags)
     (org-set-tags . helm-org-completing-read-tags)
     (ffap-alternate-file)
     (tmm-menubar)
     (find-file . helm-completing-read-symbols)
     (execute-extended-command))))
 '(ivy-dynamic-exhibit-delay-ms 500)
 '(linum-delay t)
 '(nxml-child-indent 4)
 '(package-selected-packages
   (quote
    (exec-path-from-shell fzf counsel counsel-gtags counsel-projectile ivy-todo all-the-icons-ivy ac-octave yasnippet yaml-mode web-mode smooth-scrolling smex sass-mode powerline paredit p4 org-pomodoro org-plus-contrib org nurumacs monokai-theme minimap markdown-mode magit less-css-mode jsx-mode js2-mode ido-ubiquitous helm-projectile helm-ag git-rebase-mode git-commit-mode flycheck flx-ido find-file-in-project f es-windows es-lib coffee-mode ag)))
 '(projectile-project-root-files
   (quote
    ("rebar.config" "project.clj" "SConstruct" "build.sbt" "build.gradle" "Gemfile" "requirements.txt" "tox.ini" "package.json" "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml" "mix.exs" "config.blt")))
 '(tab-stop-list nil)
 '(tab-width 4)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
