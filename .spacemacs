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
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; --------------------------------------------------------
     ;; Example of useful layers you may want to use right away
     ;; Uncomment a layer name and press C-c C-c to install it
     ;; --------------------------------------------------------
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-snippets-in-popup t)
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
     deft
     docker
     emacs-lisp
     evernote
     ;; evil-cleverparens
     evil-commentary
     (go :variables
         go-tab-width 4)
     (git :variables
          git-gutter-use-fringe nil
          magit-repo-dirs '("~/work/"))
     github
     haskell
     html
     (java :variables java-backend 'meghanada)
     javascript
     markdown
     (org :variables
          org-enable-github-support t)
     puppet
     python
     restclient
     (ruby :variables
           ruby-test-runner 'rspec
           ruby-version-manager 'rbenv
           ruby-insert-encoding-magic-comment nil)
     (rust :variables
           rust-enable-racer t
           racer-cmd "/home/patrick/.cargo/bin/racer"
           racer-rust-src-path "/home/patrick/src/rust/src")
     semantic
     shell-scripts
     sql
     syntax-checking
     vimscript
     windows-scripts
     yaml
     ycmd
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(magithub evil-escape)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
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
   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `vim-powerline' and `vanilla'. The first three
   ;; are spaceline themes. `vanilla' is default Emacs mode-line. `custom' is a
   ;; user defined themes, refer to the DOCUMENTATION.org for more info on how
   ;; to create your own spaceline theme.. (default 'spacemacs)
   dotspacemacs-mode-line-theme 'spacemacs
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
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key ":"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
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
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode t
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
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
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."
  (setq clojure-docstring-fill-column 72)

  (setq exec-path-from-shell-check-startup-files nil)

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

  (push '("melpa-stable" . "stable.melpa.org/packages/") configuration-layer-elpa-archives)
  (push '(ensime . "melpa-stable") package-pinned-packages)
  )

(defun my-c-hook ()
  (c-add-style "Google" google-c-style t)
  (c-set-style "Google")
  (c-toggle-hungry-state 1)
  )

(defun my-clojure-indents ()
  ;; compojure
  (put 'defroutes 'clojure-indent-function :defn)
  (put 'GET 'clojure-indent-function 2)
  (put 'POST 'clojure-indent-function 2)
  (put 'PUT 'clojure-indent-function 2)
  (put 'DELETE 'clojure-indent-function 2)
  (put 'HEAD 'clojure-indent-function 2)
  (put 'ANY 'clojure-indent-function 2)
  (put 'context 'clojure-indent-function 2)

  ;; trapperkeeper
  (put 'defservice 'clojure-indent-function '(1 nil (1)))

  ;; slingshot (same as default try indentation in cider)
  (put 'try+ 'clojure-indent-function 0)

  ;; ?
  (put 'defui 'clojure-indent-function '(1 nil nil (1)))
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
  (set-variable 'ycmd-server-command '("python" "/home/patrick/src/ycmd/ycmd"))
  (set-variable 'ycmd-extra-conf-whitelist '("~/work/*" "~/exercism/*"))
  (set-variable 'ycmd-global-config "/home/patrick/.emacs.d/private/ycm_global_conf.py")
  (my-clojure-indents)
  (setq evil-move-beyond-eol nil)
  (global-evil-mc-mode 1)
  (setq vc-follow-symlinks t)
  (setq-default js2-basic-offset 2)
  (setq-default js-indent-level 2)

  ;; cider
  (add-hook 'cider-connected-hook #'cider-refresh)

  (setq solarized-scale-org-headlines nil)

  (setq x-select-enable-primary t)  ; emacs 24
  (setq select-enable-primary t)  ; emacs 25

  (setq evil-want-fine-undo nil)

  ;; (spacemacs/toggle-evil-cleverparens-on)
  (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)

  (setq x-select-enable-clipboard-manager nil)

  ;; default browser
  (setq-default browse-url-browser-function 'browse-url-generic
                browse-url-generic-program "firefox-developer-edition")

  ;; (spacemacs/toggle-evil-safe-lisp-structural-editing-on-register-hook-common-lisp-mode)

  (spaceline-toggle-minor-modes-off)
)


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (zenburn-theme yasnippet-snippets yapfify yaml-mode ws-butler winum web-mode web-beautify volatile-highlights vimrc-mode vi-tilde-fringe uuidgen unfill toml-mode toc-org tagedit symon string-inflection stickyfunc-enhance srefactor sql-indent spaceline-all-the-icons spaceline powerline solarized-theme smeargle slim-mode seeing-is-believing scss-mode sass-mode rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocop rspec-mode robe restclient-helm restart-emacs rbenv rake rainbow-mode rainbow-identifiers rainbow-delimiters racer pyvenv pytest pyenv-mode py-isort puppet-mode pug-mode prettier-js powershell popwin pippel pipenv pip-requirements persp-mode password-generator paradox ox-gfm overseer orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download org-bullets org-brain open-junk-file ob-restclient ob-http neotree nameless mwim mvn move-text monokai-theme mmm-mode minitest meghanada maven-test-mode markdown-toc magit-svn magit-gitflow magit-gh-pulls macrostep lorem-ipsum livid-mode skewer-mode live-py-mode link-hint json-navigator hierarchy js2-refactor js2-mode js-doc intero insert-shebang indent-guide importmagic epc ctable concurrent impatient-mode simple-httpd hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-xref helm-themes helm-swoop helm-rtags helm-pydoc helm-purpose window-purpose imenu-list helm-projectile helm-mode-manager helm-make helm-hoogle helm-gitignore helm-git-grep helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haskell-snippets haml-mode groovy-mode groovy-imports gradle-mode google-translate google-c-style golden-ratio godoctor go-tag go-rename go-impl go-guru go-gen-test go-fill-struct go-eldoc gnuplot gitignore-templates gitignore-mode github-search github-clone gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gist gh marshal logito pcache ht gh-md geeknote fuzzy flycheck-ycmd flycheck-rust flycheck-rtags flycheck-pos-tip flycheck-haskell flycheck-bashate flx-ido flx fish-mode fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-org evil-numbers evil-mc evil-matchit evil-magit magit git-commit ghub treepy graphql with-editor evil-lisp-state evil-lion evil-indent-plus evil-iedit-state iedit evil-goggles evil-exchange evil-ediff evil-commentary evil-cleverparens smartparens evil-args evil-anzu anzu ensime sbt-mode scala-mode emmet-mode elisp-slime-nav editorconfig dumb-jump doom-modeline eldoc-eval shrink-path all-the-icons memoize dockerfile-mode docker json-mode tablist magit-popup docker-tramp json-snatcher json-reformat disaster diff-hl deft define-word dante lcr flycheck dactyl-mode cython-mode counsel-projectile projectile counsel swiper ivy company-ycmd ycmd request-deferred request deferred company-web web-completion-data company-tern dash-functional tern company-statistics company-shell company-rtags rtags company-restclient restclient know-your-http-well company-quickhelp pos-tip company-go go-mode company-ghci company-ghc ghc haskell-mode company-emacs-eclim eclim company-cabal company-c-headers company-anaconda company column-enforce-mode color-identifiers-mode cmm-mode clojure-snippets clojure-cheatsheet clj-refactor inflections edn multiple-cursors paredit peg clean-aindent-mode clang-format cider-eval-sexp-fu eval-sexp-fu highlight cider sesman spinner queue pkg-info clojure-mode epl chruby centered-cursor-mode cargo markdown-mode rust-mode bundler inf-ruby browse-at-remote auto-yasnippet yasnippet auto-highlight-symbol auto-compile packed anaconda-mode pythonic f dash s aggressive-indent ace-window ace-link ace-jump-helm-line helm avy helm-core ac-ispell auto-complete popup which-key use-package pcre2el org-plus-contrib hydra font-lock+ evil goto-chg undo-tree dotenv-mode diminish bind-map bind-key async))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
