;; =============== ;;
;; package manager ;;
;; =============== ;;

(require 'package)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("elpa" . "http://tromey.com/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

; fetch the list of packages available 
(when (not package-archive-contents)
  (package-refresh-contents))

; list the packages you want
(setq package-list '(melpa better-defaults evil evil-leader surround rainbow-mode zenburn-theme smart-mode-line mmm-mode project-explorer grizzl projectile markdown-mode scss-mode php-mode clojure-mode smartparens guide-key coffee-mode))

; install the missing packages
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))
(package-initialize)


;; enable plugins
(projectile-global-mode)

;;;; evil settings
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

(if after-init-time (sml/setup)
  (add-hook 'after-init-hook 'sml/setup))

(setq clojure-defun-style-default-indent t)

;;;; file-type settings
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(defun all-css-modes() (css-mode) (rainbow-mode)) 
(add-to-list 'auto-mode-alist '("\\.css$" . all-css-modes)) 
(setq exec-path (cons (expand-file-name "/usr/local/opt/ruby/bin") exec-path))
(autoload 'scss-mode "scss-mode")
(defun all-scss-modes() (scss-mode) (rainbow-mode)) 
(add-to-list 'auto-mode-alist '("\\.scss\\'" . all-scss-modes))


;; ============== ;;
;; other settings ;;
;; ============== ;;


;; colorscheme
(load-theme 'zenburn t)

;; settings
;(global-linum-mode t)
(menu-bar-mode nil)
(tool-bar-mode nil)
(blink-cursor-mode nil)
