;;; xunitjs.el -- A front-end for xUnit.js

;; Author: Greg Smolyn

;; Uses the compile package to create a compilation mode for xUnit.js
;; Based off of JF's version for Sublime: https://gus.my.salesforce.com/0D5B0000002pIod

;; Put this file in somewhere your emacs can find it
;;   for example, in .emacs.d/lisp, and add (add-to-list 'load-path "~/.emacs.d/lisp/") to your .emacs
;; Then add (require 'xunitjs) in your .emacs

;;
;; 'M-x xunitjs' will then call xUnit for you!
;;

(require 'compile)

(defcustom xunitjs-base-path
  "/Users/gsmolyn/blt/app/main/core/"
  "Base path"
  :type 'string
  :group 'xunitjs)

(defcustom xunitjs-os
  "MacOS"
  "Operating system, one of {MacOS, Linux, Windows}"
  :type 'string
  :group 'xunitjs)

(defcustom xunitjs-engine
  "com.google_v8/3.12.3/d8"
  "Name and partial path of executable to use"
  :type 'string
  :group 'xunitjs)

(define-compilation-mode xunitjs-mode "xUnitjs"
  "xUnit.js results compilation mode"
  '())

(define-key xunitjs-mode-map (kbd "p") #'compilation-previous-error)
(define-key xunitjs-mode-map (kbd "n") #'compilation-next-error)
(define-key xunitjs-mode-map (kbd "k") '(lambda () (interactive)
                                          (let (kill-buffer-query-functions) (kill-buffer))))

(defun xunitjs/compile ()
  (let* ((cmd (concat (mapconcat 'file-name-as-directory (list "../sfdc-test/tools/javascript/external/Engines"  xunitjs-os) "")  xunitjs-engine))
         (driver (concat (file-name-as-directory "../sfdc-test/tools/javascript/external/xUnit.js") "xUnit.js.Console.js"))
         (dependency "../sfdc-test/tools/javascript/sfdc")
         (arguments (mapconcat #'shell-quote-argument (list driver "--" dependency "/strict:false" "/coverage:false" (buffer-file-name)) " "))
         (working-dir (mapconcat 'file-name-as-directory (list xunitjs-base-path "build") ""))
         (cmd-string (concat "cd " working-dir " && " cmd " " arguments)))
;    (message "XUNITJS CMD string: %s" cmd-string)
    (compilation-start
     cmd-string
     #'xunitjs-mode)))

(defun xunitjs ()
  (interactive)
  (xunitjs/compile))

(provide 'xunitjs)
