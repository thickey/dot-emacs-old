;;; clojurescript-mode.el --- Minor mode for clojurescript code
;;; https://gist.github.com/1103191

;; Copyright (C) 2011 Hugo Duncan
;;
;; Authors: Hugo Duncan

;; Version: 0.1.0
;; Keywords: languages, lisp, javascript

;; To run the compiler every time a cljs file is saved:
;;
;; (add-hook 'after-save-hook
;;       '(lambda ()
;;          (when (string-match "\.cljs$" (buffer-name))
;;           (cljs-build))))

(defgroup clojurescript-mode nil
  "A minor mode for clojurescript"
  :prefix "clojurescript-mode-"
  :group 'clojure-mode)

(define-minor-mode clojurescript-mode
  "Toggle clojurescript mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
  :init-value nil
  :lighter "cljs"
  :group 'clojurescript-mode)

(defcustom cljs-mode-cljs-path nil
  "Sets the path to the cljs compiler."
  :type 'directory
  :group 'clojurescript-mode)

(defcustom cljs-mode-compile-command "%s/bin/cljsc"
  "The compile command used by the minor mode."
  :type 'string
  :group 'clojurescript-mode)

(setq cljs-mode-compile-options nil)
(defcustom cljs-mode-compile-options nil
  "An alist of compile options."
  :type 'alist
  :group 'clojurescript-mode)

(defcustom cljs-mode-output-path "../out"
  "The output path for cljsc, relative to the root of the source tree."
  :type 'string
  :group 'clojurescript-mode)

(defun cljs-build ()
  "Compiles a buffer to javascript using the current swank session. The aim
is to allow quick compilation of cljs source in a development environment.
The production environment is better serverd by the use of build tools."
  (interactive)
  (cljs-mode-build-buffer (current-buffer)))

(defun cljs-mode-build-buffer (buffer)
  "Compiles a buffer to javascript using the build function."
  (let ((file-name (buffer-file-name buffer)))
    (slime-eval-async
        `(swank:listener-eval
          ,(concat
            "(do
                 (require 'cljs.closure)
                 (cljs.closure/build \""
            file-name
            "\" (hash-map "
            (mapconcat
             'prin1-to-string
             (cljs-mode-compile-options-args
              (cljs-mode-output-args buffer))
             " ") ")))")))))

(defun cljs-compile ()
  "Compiles a buffer to javascript using the command line compiler."
  (interactive)
  (cljs-mode-compile-buffer (current-buffer)))

(defun cljs-mode-compile-buffer (buffer)
  "Compiles a buffer to javascript"
  (let ((file-name (buffer-file-name buffer)))
    (apply
     'start-process
     "cljsc" "*cljsc*"
     (format cljs-mode-compile-command cljs-mode-cljs-path)
     file-name
     (mapcar
      'prin1-to-string
      (cljs-mode-in-braces
       (cljs-mode-compile-options-args
        (cljs-mode-output-args buffer)))))))

(defun cljs-mode-output-args (buffer)
  "Calculate the output args for cljsc. Ensures referenced directories exist."
  (let* ((file-name (buffer-file-name buffer))
         (ns-path (cljs-mode-ns-path
                   (cljs-mode-buffer-ns buffer)))
         (source-root (cljs-mode-source-root file-name ns-path))
         (output-root (cljs-mode-output-root source-root))
         (output-file (concat output-root "/" ns-path ".js")))
    (make-directory (file-name-directory output-file) t)
    (make-directory output-root t)
    (list :output-to output-file :output-dir output-root)))

(defun cljs-mode-compile-options-args (extra-args)
  "Build a list of args for the compile options. extra-args is an alist."
  (append
   (mapcan (lambda (x) (list (car x) (cdr x))) cljs-mode-compile-options)
   extra-args))

(defun cljs-mode-in-braces (l)
  "Add brace symbols around a list"
  (append '({) l '(})))

(defun cljs-mode-output-root (source-root)
  "cljsc requires an explicit output directory and file name to do something
reasonable, and this tries to make some reasonable guesses for these."
  (concat source-root cljs-mode-output-path))

(defun cljs-mode-source-root (file-name ns-path)
  "Calculate the root of the cljs source tree, given a buffer file-name and it's
path found from the namespace."
  ;; ns-path contains no extension, so we assume .cljs and subtract 5
  (when ns-path
    (substring file-name 0 (- (length file-name) (length ns-path) 5))))

(defun cljs-mode-ns-path (namespace)
  (replace-regexp-in-string
   "\\." "/" (replace-regexp-in-string "-" "_" namespace)))

(defun cljs-mode-buffer-ns (buffer)
  "Returns the namespace of the specified buffer"
  (save-window-excursion
    (with-current-buffer buffer
      (clojure-find-ns))))

(provide 'clojurescript-mode)
;;; clojurescript-mode.el ends here
