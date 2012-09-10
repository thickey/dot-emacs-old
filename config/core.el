;; Here is the root of your personal configs.
;; Either place config straight in here,
;; such as this colour theme (feel free to change it to your own favourite theme)

;; use blackbored colour theme
(load-dotfile-lib "blackbored.el")
(color-theme-blackbored)

;;Or load external files such as this bindings file:
(load-dotfile "config/bindings.el")
(load-dotfile-lib "nrepl.el")


(setq visible-bell f)
(setq ring-bell-function 'ignore)

;;;
;;; from emacs-starter-kit
;;;
;; Save a list of recent files visited.
(recentf-mode 1)
;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(custom-set-variables
  '(org-startup-indented t)
  '(visual-line-mode nil t))

;;
;; Backup/Autosave
;; (orig. from http://jaderholm.com/configs/emacs)
;; This will keep all backup/autosafe files in one location - much cleaner.
;;
(defvar backup-dir (expand-file-name "~/tmp/.emacs.backup/"))
(defvar autosave-dir (expand-file-name "~/tmp/.emacs.autosave/"))
(make-directory backup-dir t)
(make-directory autosave-dir t)
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

(setq-default frame-title-format
              (list '((buffer-file-name " %f"
                                        (dired-directory
                                         dired-directory
                                         (revert-buffer-function " %b" ("%b - " default-directory)))))))

;;;
;;; linum mode
;;;  * http://stud4.tuwien.ac.at/~e0225855/linum/linum.el
;;;
(add-to-list 'load-path "~/.emacs.d/lib/linum/")
;;(require 'linum)
;;(global-linum-mode 1)


;;;
;;; linum-off
;;; * http://www.emacswiki.org/emacs/linum-off.el
;;;
(require 'linum-off)

;;;
;;; clojure-mode
;;;
(defun clojure-mode-setup ()
  (slime-mode t)
  (show-paren-mode t)
  (rainbow-delimiters-mode t)
  (paredit-mode t)
  (linum-mode t)
  (progn
      (define-key clojure-mode-map "\C-cc" 'comment-region)
      (define-key clojure-mode-map "\C-cu" 'uncomment-region)
;      (setq cdt-dir (expand-file-name "~/.emacs.d/site-lisp/cdt"))
;      (load-file (format "%s/ide/emacs/cdt.el" cdt-dir))
))

(autoload 'clojure-mode "clojure-mode" nil t)

(add-hook 'clojure-mode-hook #'clojure-mode-setup)
;;(add-hook 'slime-repl-mode-hook #'clojure-mode-setup)
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
;; (add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojure-mode))

;;;
;;; clojurescript-mode
;;;
(defun clojurescript-mode-setup ()
  (show-paren-mode t)
  (rainbow-delimiters-mode t)
  (paredit-mode t)
  (linum-mode t)
  (progn
      (define-key clojure-mode-map "\C-cc" 'comment-region)
      (define-key clojure-mode-map "\C-cu" 'uncomment-region)))
;; (require 'clojurescript-mode)
;; (autoload 'clojurescript-mode "clojurescript-mode" nil t)

(defun slime-mode-setup ()
  (slime-mode t)
  (show-paren-mode t)
  (paredit-mode t)
)
(add-hook 'slime-repl-mode-hook #'slime-mode-setup)

(defun nrepl-mode-setup ()
  ;; (nrepl-mode t)
  (show-paren-mode t)
  (paredit-mode t)
  ;; (clojure-mode t)
)
(add-hook 'nrepl-mode-hook #'nrepl-mode-setup)

(defun start-clojurescript ()
  (interactive)
  ;;(setq inferior-lisp-program "/Users/ffailla/dev/clojurescript/script/cljsrepl")
  (cd "/Users/tomhickey/dev/cljs/")
  (run-lisp "script/repljs"))

;; (add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-mode))


;;
;; fonts
;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
'(default ((t (:inherit nil :stipple nil :background nil :foreground nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "apple" :family "Menlo")))))

;; (set-face-attribute 'default nil :height 120)
;; (set-face-attribute 'default nil :foundry "apple")
;; (set-face-attribute 'default nil :family "Menlo")
;; (setq-default line-spacing 1) ;; 1 pixel
(setq-default line-spacing 0.1) ;; 10% of line-height

;; fontifying *clime-compilation is a killer http://random-state.net/log/3512630740.html
(setq font-lock-verbose nil)





;;
;; from David Nolen
;;
;; -----------------------------------------------------------------------------
;; JavaScript
;; -----------------------------------------------------------------------------
(load-dotfile "lib/js2/js2.elc")

(require 'js2-mode)
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(autoload 'espresso-mode "espresso")

;; (add-to-list 'load-path "~/elisp/moz.el")
;; (autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

(defun my-js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (espresso--proper-indentation parse-status))
           node)

      (save-excursion

        ;; I like to indent case and labels to half of the tab width
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ espresso-indent-level 2))))

        ;; consecutive declarations in a var statement are nice if
        ;; properly aligned, i.e:
        ;;
        ;; var foo = "bar",
        ;;     bar = "foo";
        (setq node (js2-node-at-point))
        (when (and node
                   (= js2-NAME (js2-node-type node))
                   (= js2-VAR (js2-node-type (js2-node-parent node))))
          (setq indentation (+ 2 indentation))))

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))

(defun my-indent-sexp ()
  (interactive)
  (save-restriction
    (save-excursion
      (widen)
      (let* ((inhibit-point-motion-hooks t)
             (parse-status (syntax-ppss (point)))
             (beg (nth 1 parse-status))
             (end-marker (make-marker))
             (end (progn (goto-char beg) (forward-list) (point)))
             (ovl (make-overlay beg end)))
        (set-marker end-marker end)
        (overlay-put ovl 'face 'highlight)
        (goto-char beg)
        (while (< (point) (marker-position end-marker))
          ;; don't reindent blank lines so we don't set the "buffer
          ;; modified" property for nothing
          (beginning-of-line)
          (unless (looking-at "\\s-*$")
            (indent-according-to-mode))
          (forward-line))
        (run-with-timer 0.5 nil '(lambda(ovl)
                                   (delete-overlay ovl)) ovl)))))

(defun my-js2-mode-hook ()
  (require 'espresso)
  (yas/minor-mode 1)
	(linum-mode t)
  ;; (moz-minor-mode 1)
  (setq espresso-indent-level 2
        indent-tabs-mode nil
        c-basic-offset 2)
  (c-toggle-auto-state 0)
  (c-toggle-hungry-state 1)
  (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
  (define-key js2-mode-map [(meta control |)] 'cperl-lineup)
  (define-key js2-mode-map [(meta control \;)]
    '(lambda()
       (interactive)
       (insert "/* -----[ ")
       (save-excursion
         (insert " ]----- */"))
       ))
  (define-key js2-mode-map [(return)] 'newline-and-indent)
  (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
  (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
  (define-key js2-mode-map [(control meta q)] 'my-indent-sexp)
  (if (featurep 'js2-highlight-vars)
    (js2-highlight-vars-mode))
  (message "My JS2 hook"))

;; (require 'js-comint)
;; (setq inferior-js-program-command "/usr/local/bin/js -j")
;; (add-hook 'js2-mode-hook '(lambda ()
;;			    (local-set-key "\C-x\C-e" 'js-send-last-sexp)
;;			    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
;;			    (local-set-key "\C-cb" 'js-send-buffer)
;;			    (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
;;			    (local-set-key "\C-cl" 'js-load-file-and-go)
;;			    ))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)


(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

(defun maximize-frame ()
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 1000 1000))
(maximize-frame)


;;ac-nrepl auto-complete plugin
(load-dotfile-lib "ac-nrepl.el")
(require 'ac-slime)
(add-hook 'nrepl-mode-hook 'set-up-slime-ac)
;; (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
;;     (require 'ac-nrepl)
;;     (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'clojure-nrepl-mode-hook 'ac-nrepl-setup)
;;     (eval-after-load "auto-complete"
;;       '(add-to-list 'ac-modes 'nrepl-mode))
(setq split-height-threshold nil)
