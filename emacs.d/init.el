;; =============== ;;
;; package manager ;;
;; =============== ;;

(add-to-list 'load-path "~/.emacs.d/")

(require 'package)
(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(require 'cask "~/bin/cask/cask.el")
(cask-initialize)


;; =============== ;;
;; plugin settings ;;
;; =============== ;;

;; projectile settings
(projectile-global-mode)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

;; smart-mode-line settings
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
  "f" 'ace-jump-char-mode)
(evil-mode t)
(global-surround-mode 1)
(global-rainbow-delimiters-mode)

;; file-type settings
;;; php
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
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

;; display
;(require 'whitespace)
;(global-whitespace-mode)
(global-linum-mode t)
(setq linum-format "%d ")

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

;; (multi-)term
(setq multi-term-program "/bin/sh")
(setq system-uses-terminfo nil)
(setq shell-file-name "/bin/sh")

;; random
(set-buffer-file-coding-system 'unix)
(setq inhibit-startup-screen t)


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
