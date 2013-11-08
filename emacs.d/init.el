;; =============== ;;
;; package manager ;;
;; =============== ;;

(require 'package)
(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

; fetch the list of packages available 
(when (not package-archive-contents)
  (package-refresh-contents))

; list the packages you want
(setq package-list '(better-defaults evil evil-leader surround mmm-mode project-explorer projectile markdown-mode scss-mode php-mode clojure-mode smartparens guide-key coffee-mode rainbow-delimiters flx-ido multiple-cursors rainbow-mode smart-mode-line))

; install the missing packages
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))
(package-initialize)


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
  "B" 'projectile-switch-to-buffer)
(evil-mode t)
(global-surround-mode 1)
(global-rainbow-delimiters-mode)

;; file-type settings
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(defun all-css-modes() (css-mode) (rainbow-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . all-css-modes))
(setq exec-path (cons (expand-file-name "/usr/local/opt/ruby/bin") exec-path))
(autoload 'scss-mode "scss-mode")
(defun all-scss-modes() (scss-mode) (rainbow-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . all-scss-modes))
(setq clojure-defun-style-default-indent t)


;; ============== ;;
;; other settings ;;
;; ============== ;;

;(require 'whitespace)
;(global-whitespace-mode)

;; colorscheme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-theme-gruvbox")
(load-theme 'gruvbox t)

;; line numbers
(global-linum-mode t)
(setq linum-format "%d ")

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

