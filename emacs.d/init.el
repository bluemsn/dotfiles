;; =============== ;;
;; package manager ;;
;; =============== ;;

(add-to-list 'load-path "~/.emacs.d/")

(require 'package)
(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(require 'cask "~/bin/cask/cask.el")
(cask-initialize)


;; =============== ;;
;; plugin settings ;;
;; =============== ;;

(global-hl-line-mode t)
(global-surround-mode 1)
(global-rainbow-delimiters-mode)
(smartparens-global-mode t)
(show-paren-mode 0) ;;disable built-in pair finding
(show-smartparens-global-mode t) ;;enable Smartparens pair finding
(require 'project-explorer)

;; projectile settings
(projectile-global-mode)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

;;; smart-mode-line settings
;(setq sml/theme 'respectful)
;(require 'smart-mode-line)
;(sml/setup)

;; evil settings
(setq evil-search-module 'evil-search
      evil-want-C-u-scroll t
      evil-want-C-w-in-emacs-state t
      evil-default-cursor t)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "b" 'projectile-find-file
  "B" 'ace-jump-buffer
  "o" 'prelude-open-with
  "f" 'ace-jump-char-mode
  "F" 'ace-jump-line-mode)
(evil-mode t)
(load "elscreen" "ElScreen" t)
(define-key evil-normal-state-map (kbd "C-w t") 'elscreen-create) ;creat tab
(define-key evil-normal-state-map (kbd "C-w x") 'elscreen-kill) ;kill tab
(define-key evil-normal-state-map "gT" 'elscreen-previous) ;previous tab
(define-key evil-normal-state-map "gt" 'elscreen-next) ;next tab
(defadvice evil-ex-search-next (after advice-for-evil-ex-search-next activate)
  (evil-scroll-line-to-center (line-number-at-pos)))

;; file-type settings
;;; php
(autoload 'php-mode "php-mode" t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
;;; css
(defun all-css-modes() (css-mode) (rainbow-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . all-css-modes))
;;; scss
(setq exec-path (cons (expand-file-name "/usr/local/opt/ruby/bin") exec-path))
(autoload 'scss-mode "scss-mode")
(defun all-scss-modes() (scss-mode) (rainbow-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . all-scss-modes))
;;; clojure
(setq clojure-defun-style-default-indent t)


;; ============== ;;
;; other settings ;;
;; ============== ;;

;; line numbers
(global-linum-mode t)
(setq linum-relative-current-symbol "")
(if window-system
  (setq linum-relative-format "%3s|")
  (setq linum-relative-format "%3s| "))
(require 'linum-relative)

;; whitespace
;(require 'whitespace)
;(global-whitespace-mode)

;; colorscheme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-theme-gruvbox")
(load-theme 'gruvbox t)

;; mode line
(display-time-mode t)
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
;(setq mode-line-format
;  (list))

(menu-bar-mode nil)
(tool-bar-mode nil)
(blink-cursor-mode nil)

;; tab/indent settings
(setq-default default-tab-width 4) ; make tab 4 chars wide
(setq-default tab-stop-list (number-sequence 4 200 4))
(setq-default indent-tabs-mode t)
(global-set-key (kbd "TAB") 'self-insert-command)
(setq c-backspace-function 'backward-delete-char)
(setq default-truncate-lines t)

;; (multi-)term
(setq multi-term-program "/bin/sh")
(setq system-uses-terminfo nil)
(setq shell-file-name "/bin/sh")

;; random
(set-buffer-file-coding-system 'unix)
(setq inhibit-startup-screen t) ;; disable opening screen
(when (window-system)
  (tool-bar-mode -1) ;; disable toolbar
  (scroll-bar-mode -1)) ;; disable scrollbars

;; fix the PATH variable
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))


;; fonts
(set-face-attribute 'default nil
                    :family "Inconsolata"
                    :height 140
                    :weight 'normal
                    :width 'normal)

(when (functionp 'set-fontset-font)
  (set-fontset-font "fontset-default"
                    'unicode
                    (font-spec :family "DejaVu Sans Mono"
                               :width 'normal
                               :size 12.4
                               :weight 'normal)))

;; put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;; ========= ;;
;; functions ;;
;; ========= ;;

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
