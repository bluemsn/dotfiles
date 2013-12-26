;; Package manager
;; ===============

(require 'cask "~/.emacs.d/cask/cask.el")
(cask-initialize)

;; Plugins' settings
;; =---------------=

;; Clojure
(setq clojure-defun-style-default-indent t)

;; (multi-)term
(setq multi-term-program "/bin/sh")
(setq system-uses-terminfo nil)
(setq shell-file-name "/bin/sh")

;; linum-relative
(setq linum-relative-current-symbol "")
(if window-system
  (setq linum-relative-format "%3s|")
  (setq linum-relative-format "%3s| "))

;; Evil (Emacs vi layer) and related
;; ---------------------------------

(evil-mode 1)

(setq evil-search-module 'evil-search
      evil-want-C-u-scroll t
      evil-want-C-w-in-emacs-state t
      evil-default-cursor t)
(defadvice evil-ex-search-next (after advice-for-evil-ex-search-next activate)
  (evil-scroll-line-to-center (line-number-at-pos))) ;; Center after jumping to next search match

;; Evil keybindings
(define-key evil-normal-state-map (kbd "C-w t") 'elscreen-create) ;; Create tab
(define-key evil-normal-state-map (kbd "C-w x") 'elscreen-kill) ;; Kill tab
(define-key evil-normal-state-map "gT" 'elscreen-previous) ;; Previous tab
(define-key evil-normal-state-map "gt" 'elscreen-next) ;; Next tab

;; evil-leader
(global-evil-leader-mode 1)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "b" 'projectile-find-file
  "B" 'ace-jump-buffer
  "o" 'prelude-open-with
  "f" 'ace-jump-char-mode
  "F" 'ace-jump-line-mode)


;; Other settings
;; ==============

;; Colorscheme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-theme-gruvbox")
(load-theme 'gruvbox t)

;; Tab/indent settings
(setq-default default-tab-width 4) ;; Make tab 4 chars wide
(setq-default tab-stop-list (number-sequence 4 200 4))
(setq-default indent-tabs-mode t) ;; Indent with tabs not spaces
(global-set-key (kbd "TAB") 'self-insert-command)
(setq c-backspace-function 'backward-delete-char)
(setq default-truncate-lines t)

(set-buffer-file-coding-system 'unix) ;; Set encoding to LF
(setq inhibit-startup-screen t) ;; Disable the friendly startup screen
(defalias 'yes-or-no-p 'y-or-n-p) ;; y/n instead of yes/no

;; Fix the PATH variable
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/
(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; Create the autosave dir if necessary, since emacs won't by itself (it should)
(make-directory "~/.emacs.d/autosaves/" t)

;; Functions
;; ---------

;; http://batsov.com/articles/2011/11/12/emacs-tip-number-2-open-file-in-external-program/
(defun prelude-open-with ()
  "Simple function that allows us to open the underlying
file of a buffer in an external program."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if (eq system-type 'darwin)
                        "open"
                      (read-shell-command "Open current file with: "))
                    " "
                    buffer-file-name))))


;; Modes
;; =====

;; Minor modes
;; =---------=

;; GUI stuff
(blink-cursor-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Modeline
(display-time-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

;; Line numbers
(global-linum-mode 1)
(require 'linum-relative)
(global-hl-line-mode 1) ;; Highlight cursor line

;;; Whitespace
;(require 'whitespace)
;(global-whitespace-mode)

;; Parentheses
(global-surround-mode 1)
(global-rainbow-delimiters-mode 1)
(show-paren-mode -1) ;; Disable built-in pair finding
(smartparens-global-mode 1)
(show-smartparens-global-mode 1) ;; Enable Smartparens pair finding

;; Projectile settings
(projectile-global-mode 1)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)


;; Major modes
;; =---------=

;;^ (evil-mode 1)
;;^ (global-evil-leader-mode 1)

;; SCSS
(setq exec-path (cons (expand-file-name "/usr/local/opt/ruby/bin") exec-path))
(autoload 'scss-mode "scss-mode")
(defun all-scss-modes() (scss-mode) (rainbow-mode)) ;; Enable rainbow-mode
(add-to-list 'auto-mode-alist '("\\.scss\\'" . all-scss-modes))
(setq scss-compile-at-save nil) ;; Disable compilation on save

;; PHP
(autoload 'php-mode "php-mode" t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; CSS
(defun all-css-modes() (css-mode) (rainbow-mode)) ;; Enable rainbow-mode
(add-to-list 'auto-mode-alist '("\\.css$" . all-css-modes))



;; GUI version settings
;; ====================

;; Font
(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :height 100
                    :weight 'normal
                    :width 'normal)

(when window-system (set-exec-path-from-shell-PATH))
