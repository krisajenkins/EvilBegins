;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Version check.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (< emacs-major-version 24)
  (error "This setup requires Emacs v24, or higher. You have: v%d" emacs-major-version))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packaging setup.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(package-initialize)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(defvar my-packages '(evil 
		      evil-leader surround
		      elscreen ace-jump-mode
		      helm
		      key-chord
		      recentf
		      rainbow-delimiters highlight paredit smartparens
		      clojure-mode clojure-test-mode clojure-cheatsheet
		      nrepl nrepl-eval-sexp-fu ac-nrepl
		      )
  "A list of packages to check for and install at launch.")

(defun my-packages-missing-p ()
  (let ((missing-p nil)) ()
       (dolist (package my-packages missing-p)
	 (or (package-installed-p package)
	     (setq missing-p t)))))

(when (my-packages-missing-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (package my-packages)
    (when (not (package-installed-p package))
      (package-install package))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customizations (from M-x customze-*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(nrepl-popup-stacktraces-in-repl t)
 '(nrepl-hide-special-buffers t)
 '(recentf-max-saved-items 50))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic Vim Emulation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-mode t)
(elscreen-start)

(evil-define-key 'normal global-map
  "gt" 'elscreen-next
  "gT" 'elscreen-previous)

(evil-ex-define-cmd "tabc[lose]" 'elscreen-kill)
(evil-ex-define-cmd "tabn[ew]" 'elscreen-create)
(evil-ex-define-cmd "Exp[lore]" 'dired-jump)
; TODO Map :Exp to helm/dired/diredp?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Nice-to-haves...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-surround-mode t)
(smartparens-global-mode t)

;;; Ctrl-P-esq.
(helm-mode t)
(recentf-mode t)
(evil-define-key 'normal global-map
  "\C-p" 'helm-mini)

;;; Ace Jump
(evil-define-key 'normal global-map
  "\\\\w" 'evil-ace-jump-word-mode)

;;; Uncomment these key-chord lines if you like that "remap 'jk' to ESC" trick.
;; (key-chord-mode t)
;; (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filetype-style hooks.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     (require 'nrepl-eval-sexp-fu)
	     (rainbow-delimiters-mode t)))

(evil-define-key 'normal emacs-lisp-mode-map
  "\M-q" 'sp-indent-defun
  "K" '(lambda ()
	 (interactive)
	 (describe-function (symbol-at-point))))

;;; Clojure
(add-hook 'clojure-mode-hook
	  '(lambda ()
	     (require 'nrepl-eval-sexp-fu)
	     (rainbow-delimiters-mode t)

	     (mapc '(lambda (char)
		      (modify-syntax-entry char "w" clojure-mode-syntax-table))
		   '(?- ?_ ?/ ?< ?> ?: ?' ?.))
  
	     (require 'clojure-test-mode)
  
	     (require 'ac-nrepl)
	     (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
	     (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
	     (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
	     (add-to-list 'ac-modes 'nrepl-mode)))

(evil-define-key 'normal clojure-mode-map
  "\M-q" 'sp-indent-defun
  "gK" 'nrepl-src
  "K"  'ac-nrepl-popup-doc)
