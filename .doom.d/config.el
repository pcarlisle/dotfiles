;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Source Code Pro" :size 17 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "open sans" :size 18))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Allow me to manage cider dependencies myself
(setq cider-inject-dependencies-at-jack-in nil)

(setq which-key-idle-delay 0.25)

(setq flycheck-ruby-rubocop-executable "~/.rbenv/shims/rubocop")
(add-hook 'ruby-mode-hook
  (lambda ()
    (setq-local flycheck-command-wrapper-function
                (lambda (command) (append '("bundle" "exec") command)))))


;; Seems to fix hanging at quit
(setq x-select-enable-clipboard-manager nil)

;; (after! lispyville
;;   (define-key lispy-mode-map-lispy "[" nil)
;;   (define-key lispy-mode-map-lispy "]" nil))

(setq doom-scratch-dir "~/.scratch")
(setq doom-scratch-initial-major-mode 'org-mode)

(setq +format-on-save-enabled-modes '(go-mode))

(setq lsp-go-codelenses
      '((gc_details . t)
        (generate . t)
        (regenerate_cgo . t)
        (tidy . t)
        (upgrade_dependency . t)
        (test . t)
        (vendor . t)))

(after! smartparens
  ;; (add-hook! clojure-mode #'smartparens-strict-mode)

  (setq evil-cleverparens-use-s-and-S nil)

  (use-package! evil-cleverparens
    :init
    (setq evil-move-beyond-eol t
          evil-cleverparens-use-additional-bindings t
          ;; evil-cleverparens-swap-move-by-word-and-symbol t
          ;; evil-cleverparens-use-regular-insert t
          )

    (add-hook! clojure-mode #'evil-cleverparens-mode)
    ;; (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
    ))

(map! :after evil-cleverparens
      :map clojure-mode-map
      :localleader
      (:desc "Wrap round" :n "(" #'sp-wrap-round
       :desc "Wrap square" :n "[" #'sp-wrap-square
       :desc "Wrap curly" :n "{" #'sp-wrap-curly
       :desc "Unwrap sexp" :n "u" #'sp-unwrap-sexp))


(use-package! evil-lisp-state
  :custom
  (evil-lisp-state-global t)
  :config (evil-lisp-state-leader "SPC k"))


;; Fix running other checkers after lsp
;; fix from https://github.com/hlissner/doom-emacs/issues/1530
;; flycheck issue https://github.com/flycheck/flycheck/issues/1762
(add-hook! 'lsp-after-initialize-hook
  (run-hooks (intern (format "%s-lsp-hook" major-mode))))

(defun go-flycheck-setup ()
  (flycheck-add-next-checker 'lsp 'golangci-lint))

(defun sh-flycheck-setup ()
  (flycheck-add-next-checker 'lsp 'sh-shellcheck))

(add-hook 'go-mode-lsp-hook
          #'go-flycheck-setup)

(add-hook 'sh-mode-lsp-hook
          #'sh-flycheck-setup)

(setq lsp-go-env '((GOFLAGS . "-tags=integration")))

(setq ns-command-modifier 'meta)
;; Somehow this is being interpreted backwards so set left alt to super and right alt to inputting weird characters
(setq ns-right-alternate-modifier 'super)
(setq ns-alternate-modifier 'none)
