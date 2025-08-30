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

(setq password-cache-expiry 300)

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

;; handy function for remote session
(defun remote-combo-session (user host)
  "Open new tab with TRAMP Dired and vterm, rename vterm buffer."
  (interactive "sUser: \nsHost: ")
  (let ((remote-path (format "/ssh:%s@%s:~/" user host))
        dired-buf
        vterm-buf
        tab-name)
    (setq tab-name (format "remote-%s@%s" user host))
    ;; Створити dired буфер
    (setq dired-buf (dired-noselect remote-path))
    (with-current-buffer dired-buf
      (rename-buffer (format "[remote] Dired %s@%s" user host)))
    ;; Відкрити нову вкладку
    (tab-bar-new-tab)
    (tab-bar-rename-tab tab-name)
    (delete-other-windows)
    (split-window-right)
    ;; Ліве вікно — dired
    (set-window-buffer (selected-window) dired-buf)
    ;; Праве вікно — vterm з default-directory
    (other-window 1)
    (let ((default-directory remote-path))
      (vterm)
      ;; Переіменовуємо buffer після створення
      (setq vterm-buf (current-buffer))
      (with-current-buffer vterm-buf
        (rename-buffer (format "[remote] vterm %s@%s" user host))))
    ;; Повернути фокус на dired
    (other-window -1)))



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
  :disabled t
  :ensure t
  :config
  (dashboard-setup-startup-hook))

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
  :disabled t
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

(use-package magit
  :ensure t)

(use-package vterm
  :ensure t)

(use-package gptel
  :ensure t
  :config
  (gptel-make-gh-copilot "Copilot")
  (setq gptel-default-mode 'text-mode)
  (setq gptel-model 'gpt-4.1
        gptel-backend (gptel-make-gh-copilot "Copilot")))

;; To consider in future: consult, marginalia

;; holo-layer for cursor animations
(add-to-list 'load-path (concat user-emacs-directory "holo-layer/"))
(require 'holo-layer)
;; (holo-layer-enable)
(add-hook 'emacs-startup-hook
          (lambda ()
            (when (display-graphic-p)
              (require 'holo-layer)
              (holo-layer-enable))))
(setq holo-layer-enable-cursor-animation t)
(setq holo-layer-cursor-alpha 100)
(setq holo-layer-cursor-animation-color-gradient-start-value 100)
(setq holo-layer-cursor-animation-interval 10)

(setq holo-layer-enable-type-animation t)
(setq holo-layer-type-animation-style "firefly")

(with-eval-after-load 'holo-layer
  ;; Block animation for certain commands
  (setq holo-layer-cursor-block-commands
        '(
          ;; Movement
          ;; "next-line" "previous-line"
          "forward-char" "backward-char"
          "right-char" "left-char"
          ;; "forward-word" "backward-word"
          ;; "beginning-of-line" "end-of-line"

          ;; Typing
          "self-insert-command"
          
          ;; Deleting
          "delete-backward-char" "delete-forward-char"
          "backward-delete-char-untabify"
          
          ;; Company-mode navigation
          "company-select-next" "company-select-previous"
          "company-complete-selection"
          "company-abort")))

;; Tried eca-emacs - too young project
;; (use-package eca
;;   :ensure t
;;   :config
;;   (setq eca-custom-command nil))
