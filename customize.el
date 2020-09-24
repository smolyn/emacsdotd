(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(counsel-async-filter-update-time 1000)
 '(helm-completing-read-handlers-alist
   '((describe-function . helm-completing-read-symbols)
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
	 (execute-extended-command)))
 '(ivy-dynamic-exhibit-delay-ms 500)
 '(linum-delay t)
 '(nxml-child-indent 4)
 '(package-selected-packages
   '(diminish company-lsp which-key lsp-treemacs treemacs treemacs-all-the-icons treemacs-magit treemacs-projectile helm-lsp lsp-p4 lsp-ui lsp-ivy company tide ws-butler exec-path-from-shell fzf counsel counsel-gtags counsel-projectile ivy-todo all-the-icons-ivy ac-octave yasnippet yaml-mode web-mode smooth-scrolling smex sass-mode powerline paredit p4 org-pomodoro org-plus-contrib org nurumacs monokai-theme minimap markdown-mode magit less-css-mode jsx-mode js2-mode ido-ubiquitous helm-projectile helm-ag git-rebase-mode git-commit-mode flycheck flx-ido find-file-in-project f es-windows es-lib coffee-mode ag))
 '(projectile-project-root-files
   '("rebar.config" "project.clj" "SConstruct" "build.sbt" "build.gradle" "Gemfile" "requirements.txt" "tox.ini" "package.json" "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml" "mix.exs" "config.blt"))
 '(tab-stop-list nil)
 '(tab-width 4)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
