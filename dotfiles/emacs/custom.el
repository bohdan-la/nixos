;; Removing unneccesary
(setq inhibit-startup-screen t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)
(setq ring-bell-function 'ignore)

;; Loading color scheme
(load-theme 'solarized-light t)
;; Other schemes to try


;; Setting font
(set-face-attribute 'default nil
                    :family "0xProto Nerd Font Propo"
                    :height 105)

;; Tweaking whitespaces-mode
(setq whitespace-style '(face trailing lines spaces space-mark))

;; Config tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Ability to show recent files with M-x recentf-open-files
(recentf-mode 1)

;; Remembering minibuffer prompt history
(setq history-length 25)
(savehist-mode 1)

;; Remembering the last place visited in a file
(save-place-mode 1)

;; Save session upon exit
(desktop-save-mode 1)

;; Line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; Automatically revert buffers for changed files outside of Emacs
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Auto-save and backups in emacs config directory
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-save/") t))) 
(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "backups/"))))

;; Redirect auto-save files for sudo and remote files to a local dir
(setq tramp-auto-save-directory (concat user-emacs-directory "tramp-autosaves/"))
(make-directory tramp-auto-save-directory t)

;; Easy movement by paragraphs
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-o") 'other-window)

;; Smooth scrolling
(setq pixel-scroll-precision-mode t)

;; Windows history
(winner-mode)

;; Toggle function: show if hidden, hide if visible
(defun my/toggle-eshell-split ()
  "Toggle the visibility of the *eshell-split* buffer."
  (interactive)
  (let ((win (get-buffer-window "*eshell-split*")))
    (if win
        (delete-window win)
      (let ((buf (get-buffer "*eshell-split*")))
        (split-window-right)
        (other-window 1)
        (if buf
            (switch-to-buffer buf) ;; Show hidden buffer
          (progn
            (eshell)
            (rename-buffer "*eshell-split*" t))))))) ;; Buffer doesn't exist: create and rename

;; Keybinding: C-c e toggles the split eshell
(global-set-key (kbd "C-c e") #'my/toggle-eshell-split)

(setq password-cache-expiry 300)

;; Package system
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package if not already
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Better completion
(use-package vertico
  :init (vertico-mode))

;; Completion style that matches multiple regexps in any order
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Centering buffer horizontaly with olivetti-mode
(use-package olivetti)

;; Dim inactive windows
(use-package dimmer)
(dimmer-mode)

;; Dashboard
;; use-package with package.el:
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;; company-mode
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)) ;; show completions immediately

;; LSP setup
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (nix-mode . lsp-deferred)
  :commands (lsp lsp-deferred))

(with-eval-after-load 'lsp-mode
  (lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection "nixd")
                     :major-modes '(nix-mode)
                     :priority 1
                     :server-id 'nixd)))

(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "nil")
    :major-modes '(nix-mode)
    :priority 0
    :server-id 'nil
    :add-on? t))) ;; allows running nil server in pair with nixd

(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-ui-doc-enable nil)

;; company-mode
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)) ;; show completions immediately

(use-package nix-mode
  :ensure t
  :after lsp-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'")
  :hook (nix-mode . lsp-deferred)
; :custom (lsp-disabled-clients '((nix-mode . nix-nil)))
  :config
  (setq lsp-nix-nixd-server-path "nixd"
        lsp-nix-nixd-formatting-command [ "nixfmt" ]
        lsp-nix-nixd-nixpkgs-expr "import <nixpkgs> { }"
        lsp-nix-nixd-nixos-options-expr
        "(builtins.getFlake \"/etc/nixos\").nixosConfigurations.nixos.options"))

(setq lsp-completion-provider :capf)

;; This allows you to avoid many PATH glitches
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; To consider in future: consult, marginalia
