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
(setq package-list '(melpa better-defaults undo-tree evil-leader evil surround rainbow-mode zenburn-theme smart-mode-line))

; install the missing packages
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))
(package-initialize)

;; evil settings
(setq evil-search-module 'evil-search
      evil-want-C-u-scroll t
      evil-want-C-w-in-emacs-state t
      evil-default-cursor t)

;; enable plugins
;(projectile-global-mode)
(global-evil-leader-mode)
(evil-mode t)
(global-surround-mode 1)

(if after-init-time (sml/setup)
  (add-hook 'after-init-hook 'sml/setup))

;; colorscheme
(load-theme 'zenburn t)

;; settings
(global-linum-mode t)
(menu-bar-mode nil)
(tool-bar-mode nil)
(blink-cursor-mode nil)
