;; Place personal bindings here

;; https://sites.google.com/site/steveyegge2/effective-emacs
;; Invoke M-x without the Alt key
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
;; Prefer backward-kill-word over Backspace
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(defalias 'ttl 'toggle-truncate-lines)
(defalias 'rnb 'rename-buffer)
(defalias 'slc 'slime-connect)

;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
;;(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
;;(global-set-key (kbd "C-c y") 'bury-buffer)
;;(global-set-key (kbd "C-c r") 'revert-buffer)
;;(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
;;(global-set-key (kbd "C-x C-b") 'ibuffer)