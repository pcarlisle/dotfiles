;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.
;; (setq max-lisp-eval-depth 50)
;; (setq debug-on-error t)

;; Ripped wholesale from google and slightly modified
;; https://github.com/google/styleguide/blob/gh-pages/google-c-style.el
;; Wrapper function needed for Emacs 21 and XEmacs (Emacs 22 offers the more
;; elegant solution of composing a list of lineup functions or quantities with
;; operators such as "add")
(defun google-c-lineup-expression-plus-4 (langelem)
  "Indents to the beginning of the current C expression plus 4 spaces.
This implements title \"Function Declarations and Definitions\"
of the Google C++ Style Guide for the case where the previous
line ends with an open parenthese.
\"Current C expression\", as per the Google Style Guide and as
clarified by subsequent discussions, means the whole expression
regardless of the number of nested parentheses, but excluding
non-expression material such as \"if(\" and \"for(\" control
structures.
Suitable for inclusion in `c-offsets-alist'."
  (save-excursion
    (back-to-indentation)
    ;; Go to beginning of *previous* line:
    (c-backward-syntactic-ws)
    (back-to-indentation)
    (cond
     ;; We are making a reasonable assumption that if there is a control
     ;; structure to indent past, it has to be at the beginning of the line.
     ((looking-at "\\(\\(if\\|for\\|while\\)\\s *(\\)")
      (goto-char (match-end 1)))
     ;; For constructor initializer lists, the reference point for line-up is
     ;; the token after the initial colon.
     ((looking-at ":\\s *")
      (goto-char (match-end 0))))
    (vector (+ 4 (current-column)))))

(defconst google-c-style
  `((c-recognize-knr-p . nil)
    (c-enable-xemacs-performance-kludge-p . t) ; speed up indentation in XEmacs
    (c-basic-offset . 4)
    (indent-tabs-mode . nil)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist . ((defun-open after)
                               (defun-close before after)
                               (class-open after)
                               (class-close before after)
                               (inexpr-class-open after)
                               (inexpr-class-close before)
                               (namespace-open after)
                               (inline-open after)
                               (inline-close before after)
                               (block-open after)
                               (block-close . c-snug-do-while)
                               (extern-lang-open after)
                               (extern-lang-close after)
                               (statement-case-open after)
                               (substatement-open after)))
    (c-hanging-colons-alist . ((case-label)
                               (label after)
                               (access-label after)
                               (member-init-intro before)
                               (inher-intro)))
    (c-hanging-semi&comma-criteria
     . (c-semi&comma-no-newlines-for-oneline-inliners
        c-semi&comma-inside-parenlist
        c-semi&comma-no-newlines-before-nonblanks))
    (c-indent-comments-syntactically-p . t)
    (comment-column . 40)
    (c-indent-comment-alist . ((other . (space . 2))))
    (c-cleanup-list . (brace-else-brace
                       brace-elseif-brace
                       brace-catch-brace
                       empty-defun-braces
                       defun-close-semi
                       list-close-comma
                       scope-operator))
    (c-offsets-alist . ((arglist-intro google-c-lineup-expression-plus-4)
                        (func-decl-cont . ++)
                        (member-init-intro . ++)
                        (inher-intro . ++)
                        (comment-intro . 0)
                        (arglist-close . c-lineup-arglist)
                        (topmost-intro . 0)
                        (block-open . 0)
                        (inline-open . 0)
                        (substatement-open . 0)
                        (statement-cont
                         .
                         (,(when (fboundp 'c-no-indent-after-java-annotations)
                             'c-no-indent-after-java-annotations)
                          ,(when (fboundp 'c-lineup-assignments)
                             'c-lineup-assignments)
                          ++))
                        (label . /)
                        (case-label . +)
                        (statement-case-open . +)
                        (statement-case-intro . +) ; case w/o {
                        (access-label . /)
                        (innamespace . 0))))
  "Google C/C++ Programming Style.")

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `!distribution'. For now available distributions are `spacemacs-core'
   ;; or `spacemacs'. (default 'spacemacs-core)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; --------------------------------------------------------
     ;; Example of useful layers you may want to use right away
     ;; Uncomment a layer name and press C-c C-c to install it
     ;; --------------------------------------------------------
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t)
     better-defaults
     (c-c++ :variables
            c-c++-enable-clang-support t
            c-c++-default-mode-for-headers 'c++-mode)
     (clojure :variables
              cljr-favor-prefix-notation nil
              cider-inject-dependencies-at-jack-in nil
              cider-overlays-use-font-lock t
              nrepl-hide-special-buffers t
              cider-connection-message-fn nil
              ;; try having overlays last indefinitely
              cider-eval-result-duration nil
              ;; cider-use-overlays 'both ;; default both
              cider-repl-use-pretty-printing t
              cider-repl-display-help-banner nil)
     colors
     emacs-lisp
     evernote
     evil-commentary
     (git :variables
          git-gutter-use-fringe nil
          magit-repo-dirs '("~/work/"))
     github
     haskell
     javascript
     markdown
     (nand2tetris :variables
                  nand2tetris-core-base-dir "~/projects/nand2tetris")
     (org :variables
          org-enable-github-support t)
     puppet
     python
     restclient
     ruby
     semantic
     shell-scripts
     sql
     syntax-checking
     windows-scripts
     yaml
     ycmd
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'doge
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(solarized-dark
                         solarized-light
                         spacemacs-dark
                         spacemacs-light
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Consolas"
                               ;; :size 26
                               :size 20
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to miminimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   ;; dotspacemacs-which-key-position 'right-then-bottom
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode t
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."
  (setq clojure-docstring-fill-column 72)

  (setq org-startup-with-inline-images t)
  (defun pbc--extract-ns-list (req-str)
    (s-split "\\."
             (car (s-split " " (cljr--extract-sexp-content req-str)))))

  (defun pbc--shared-list-length (l1 l2)
    (length
     (-take-while 'identity
                  (-zip-with 'equal l1 l2))))

  (defun pbc-clj-ns-comparator (s1 s2)
    "Sort clojure namespaces"
    (let* ((ns1 (cljr--extract-sexp-content s1))
           (ns2 (cljr--extract-sexp-content s2))
           (nslist-1 (pbc--extract-ns-list s1))
           (nslist-2 (pbc--extract-ns-list s2))
           (current-ns-list (pbc--extract-ns-list (clojure-find-ns)))
           (shared-length-s1 (pbc--shared-list-length nslist-1 current-ns-list))
           (shared-length-s2 (pbc--shared-list-length nslist-2 current-ns-list)))
      (cond ((and (s-starts-with? "clojure" ns1)
                  (s-starts-with? "clojure" ns2))
             (cljr--string-natural-comparator s1 s2))
            ((s-starts-with? "clojure" ns1) t)
            ((s-starts-with? "clojure" ns2) nil)
            (t (if (/= shared-length-s1 shared-length-s2)
                   (> shared-length-s2 shared-length-s1)
                 (cljr--string-natural-comparator s1 s2))))))

  (setq cljr-sort-comparator 'pbc-clj-ns-comparator)

  ;; Move focus to new split
  ;; (defadvice split-window (after move-point-to-new-window activate)
  ;;   "Moves the point to the newly created window after splitting."
  ;;   (other-window 1))

  (advice-add #'magit-key-mode-popup-committing :after
            (lambda ()
              (magit-key-mode-toggle-option (quote committing) "--verbose")))

  (setq mouse-yank-at-point t)

  ;; (setq-default ruby-version-manager 'rbenv)
  )

(defun my-c-hook ()
  (c-add-style "Google" google-c-style t)
  (c-set-style "Google")
  (c-toggle-hungry-state 1)
  )

(defun my-clojure-indents ()
  ;; Compojure routes
  (put 'GET 'clojure-backtracking-indent '(2))
  (put 'POST 'clojure-backtracking-indent '(2))
  (put 'PUT 'clojure-backtracking-indent '(2))
  (put 'DELETE 'clojure-backtracking-indent '(2))
  (put 'HEAD 'clojure-backtracking-indent '(2))
  (put 'ANY 'clojure-backtracking-indent '(2))
  (put 'context 'clojure-backtracking-indent '(2))

  (put 'defservice 'clojure-backtracking-indent '((2)))
  (put 'defservice 'clojure-indent-function '(1 nil nil (1)))
  (put 'defui 'clojure-indent-function '(1 nil nil (1)))
  (put 'try+ 'clojure-indent-function 0)
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  (add-hook 'prog-mode-hook 'linum-mode)
  (add-hook 'text-mode-hook 'linum-mode)
  ;; (add-hook 'prog-mode-hook 'linum-relative-toggle)
  ;; (add-hook 'text-mode-hook 'linum-relative-toggle)
  (add-hook 'c-mode-hook 'my-c-hook)
  (add-hook 'c++-mode-hook 'my-c-hook)
  (setq guide-key/popup-window-position 'right)
  (setq mouse-wheel-scroll-amount '(2)) ;; mouse scroll moves 1 line at a time, instead of 5 lines
  (setq mouse-wheel-progressive-speed nil) ;; on a long mouse scroll keep scrolling by 1 line
  (set-variable 'ycmd-server-command '("python2.7" "/home/patrick/src/ycmd/ycmd"))
  (set-variable 'ycmd-extra-conf-whitelist '("~/work/*" "~/exercism/*"))
  (set-variable 'ycmd-global-config "/home/patrick/.emacs.d/private/ycm_global_conf.py")
  (my-clojure-indents)
  (setq evil-move-beyond-eol nil)
  (global-evil-mc-mode 1)
  (global-evil-search-highlight-persist 0)
  (setq vc-follow-symlinks t)
  (setq-default js2-basic-offset 2)
  (setq-default js-indent-level 2)

  ;; cider
  (add-hook 'cider-connected-hook #'cider-refresh)

  (setq exec-path-from-shell-check-startup-files nil)
  (setq dotspacemacs-distinguish-gui-tab t)

  (setq solarized-scale-org-headlines nil)

  (setq x-select-enable-primary t)  ; emacs 24
  (setq select-enable-primary t)  ; emacs 25

  (setq evil-want-fine-undo nil)
)


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(exec-path-from-shell-check-startup-files nil)
 '(ring-bell-function (quote ignore) t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
