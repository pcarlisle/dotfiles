# Enable for profiling (and zprof line at end)
# zmodload zsh/zprof

ZINIT_HOME="${ZINIT_HOME:-${ZPLG_HOME:-${ZDOTDIR:-${HOME}}/.zinit}}"
ZINIT_BIN_DIR_NAME="${${ZINIT_BIN_DIR_NAME:-${ZPLG_BIN_DIR_NAME}}:-bin}"
### Added by Zinit's installer
if [[ ! -f "${ZINIT_HOME}/${ZINIT_BIN_DIR_NAME}/zinit.zsh" ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing DHARMA Initiative Plugin Manager (zdharma/zinit)…%f"
    command mkdir -p "${ZINIT_HOME}" && command chmod g-rwX "${ZINIT_HOME}"
    command git clone https://github.com/zdharma-continuum/zinit "${ZINIT_HOME}/${ZINIT_BIN_DIR_NAME}" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f" || \
        print -P "%F{160}▓▒░ The clone has failed.%f"
fi

# This makes sure polaris/bin is set in the path on the first run
mkdir -p "$HOME/.zinit/polaris/bin"

source "${ZINIT_HOME}/${ZINIT_BIN_DIR_NAME}/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit
### End of Zinit installer's chunk

zi light zdharma-continuum/zinit-annex-bin-gem-node
zi light zdharma-continuum/zinit-annex-patch-dl
# Load a few important annexes, without Turbo
# zi light z-shell/z-a-meta-plugins
# zi light-mode for @annexes @romkatv

if [[ -z ${SHELL} ]]; then
  export SHELL=/bin/zsh
fi

if [[ -n "$SSH_CLIENT" ]] || [[ -n "$SSH_TTY" ]] && [[ $TERM == "xterm-kitty" ]]; then
  export TERM=xterm-256color
fi

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
    source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Set emacs key bindings early so plugins can override defaults
bindkey -e

## Plugins

zi ice depth=1
zi light romkatv/powerlevel10k

zi for \
    from'gh-r' \
    sbin'jq* -> jq' \
  stedolan/jq

zi wait lucid for \
    atclone'cp -vf completions/exa.zsh _exa'  \
    from'gh-r' \
    sbin'**/exa -> exa' \
  ogham/exa

zi wait lucid for \
    from'gh-r'  \
    sbin'**/fd -> fd' \
  @sharkdp/fd

zi wait lucid for \
    from'gh-r' \
    sbin'**/delta -> delta' \
  dandavison/delta

zi wait lucid for \
    from'gh-r' \
    sbin'**/bat -> bat' \
  @sharkdp/bat

zi wait lucid for \
    from'gh-r' \
    sbin'**/rg -> rg' \
  BurntSushi/ripgrep

zi snippet OMZL::git.zsh
zi ice atload"unalias grv; unalias glo"
zi snippet OMZP::git

zi ice wait lucid
zi pack"bgn-binary+keys" for fzf

zi ice wait lucid
zi light Aloxaf/fzf-tab

zi ice wait lucid atload'bindkey "^[[A" history-substring-search-up; bindkey "^[[B" history-substring-search-down; bindkey "${terminfo[kcuu1]}" history-substring-search-up; bindkey "${terminfo[kcud1]}" history-substring-search-down'
zi light zsh-users/zsh-history-substring-search

# zi wait lucid for \
#  atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" \
#     zdharma/fast-syntax-highlighting \
#  blockf \
#     zsh-users/zsh-completions

zi wait lucid atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" light-mode for \
  zsh-users/zsh-completions

zi ice wait lucid
zi light wfxr/forgit

path=($HOME/.rbenv/bin(N-/) $path) 
if which rbenv > /dev/null; then
  zi ice wait lucid
  zi light htlsne/zinit-rbenv
fi

## Completion style
# Taken from omz completion.zsh, so far unedited
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' list-colors ''
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path $ZSH_CACHE_DIR
# Don't complete uninteresting users
zstyle ':completion:*:*:*:users' ignored-patterns \
       adm amanda apache at avahi avahi-autoipd beaglidx bin cacti canna \
       clamav daemon dbus distcache dnsmasq dovecot fax ftp games gdm \
       gkrellmd gopher hacluster haldaemon halt hsqldb ident junkbust kdm \
       ldap lp mail mailman mailnull man messagebus  mldonkey mysql nagios \
       named netdump news nfsnobody nobody nscd ntp nut nx obsrun openvpn \
       operator pcap polkitd postfix postgres privoxy pulse pvm quagga radvd \
       rpc rpcuser rpm rtkit scard shutdown squid sshd statd svn sync tftp \
       usbmux uucp vcsa wwwrun xfs '_*'
# ... unless we really want to.
zstyle '*' single-ignored show

# Set up nice ls colors - This part is from my own old zshrc
if [[ $(uname -s) == "Darwin" ]]; then
  eval "$(gdircolors)"
else
  eval "$(dircolors)"
fi

zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

## History

if [ -z "$HISTFILE" ]; then
    HISTFILE=$HOME/.zsh_history
fi

HISTSIZE=100000
SAVEHIST=100000

## Shell options
# So far this is almost entirely copied out of OMZ to preserve what I'm used to

setopt append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups # ignore duplication command history list
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history
setopt share_history # share command history data

setopt long_list_jobs #?
setopt interactivecomments #?

# From completion.zsh
unsetopt menu_complete   # do not autoselect the first completion entry
unsetopt flowcontrol
setopt auto_menu         # show completion menu on successive tab press
setopt complete_in_word
setopt always_to_end

# From directories.zsh
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushdminus

unsetopt correctall

## Key bindings
# Initially copied from omz/lib/key-bindings.zsh

# Make sure that the terminal is in application mode when zle is active, since
# only then values from $terminfo are valid
if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
  function zle-line-init() {
    echoti smkx
  }
  function zle-line-finish() {
    echoti rmkx
  }
  zle -N zle-line-init
  zle -N zle-line-finish
fi

bindkey '\ew' kill-region                             # [Esc-w] - Kill from the cursor to the mark
bindkey -s '\el' 'ls\n'                               # [Esc-l] - run command: ls
if [[ "${terminfo[kpp]}" != "" ]]; then
  bindkey "${terminfo[kpp]}" up-line-or-history       # [PageUp] - Up a line of history
fi
if [[ "${terminfo[knp]}" != "" ]]; then
  bindkey "${terminfo[knp]}" down-line-or-history     # [PageDown] - Down a line of history
fi

if [[ "${terminfo[khome]}" != "" ]]; then
  bindkey "${terminfo[khome]}" beginning-of-line      # [Home] - Go to beginning of line
fi
if [[ "${terminfo[kend]}" != "" ]]; then
  bindkey "${terminfo[kend]}"  end-of-line            # [End] - Go to end of line
fi

bindkey ' ' magic-space                               # [Space] - do history expansion

bindkey '^[[1;5C' forward-word                        # [Ctrl-RightArrow] - move forward one word
bindkey '^[[1;5D' backward-word                       # [Ctrl-LeftArrow] - move backward one word

if [[ "${terminfo[kcbt]}" != "" ]]; then
  bindkey "${terminfo[kcbt]}" reverse-menu-complete   # [Shift-Tab] - move through the completion menu backwards
fi

bindkey '^?' backward-delete-char                     # [Backspace] - delete backward
if [[ "${terminfo[kdch1]}" != "" ]]; then
  bindkey "${terminfo[kdch1]}" delete-char            # [Delete] - delete forward
else
  bindkey "^[[3~" delete-char
  bindkey "^[3;5~" delete-char
  bindkey "\e[3~" delete-char
fi

# Edit the current command line in $EDITOR
autoload -U edit-command-line
zle -N edit-command-line
bindkey '\C-x\C-e' edit-command-line

# file rename magic
bindkey "^[m" copy-prev-shell-word

## Aliases

if [[ $(uname -s) == "Darwin" ]] && which gls >/dev/null; then
  alias ls='gls --color=tty --hyperlink=auto'
else
  alias ls='ls --color=tty --hyperlink=auto'
fi

if which exa > /dev/null; then
  alias l='exa -lah --git --time-style=long-iso'
fi

if which hub > /dev/null; then
  alias git=hub
fi

if which bat > /dev/null; then
  alias cat='bat --style=plain'
fi

source ~/.zshalias

## Environment
# TODO: Organize this

export GEM_SOURCE='https://rubygems.org'
export WORKDIR='/home/patrick/work'  # I don't remember what uses this

export RUST_SRC_PATH="${HOME}/src/rust/src"

export JAVA_HOME=/usr/lib/jvm/default
export FLAMEGRAPH_DIR="${HOME}/work/FlameGraph"

export PAGER=less

# Less options:
# -
# -M : long prompt (???)
# -S : don't wrap long lines
# -x4 : Display tab characters as 4 spaces
# -R : Pass through color control codes (allows display of colors)
# -X : Don't init/deinit terminal (leave display on screen on exit)
# -F : automatically exit if display fits entirely on one screen
export LESS="-iMSRXF -x4" 

export GDK_USE_XFT=1
export QT_XFT=true

unset RUBYOPT

export BLOCKSIZE=K
export EDITOR=vim

export RIPGREP_CONFIG_PATH="${HOME}/.ripgreprc"

export FORGIT_FZF_DEFAULT_OPTS="--exact"

export __GL_SHADER_DISK_CACHE_SKIP_CLEANUP=1

if [[ -e "/Applications/Emacs.app/Contents/MacOS/Emacs" ]]; then
  export EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"
fi

export RANTLY_VERBOSE=0

export GOPROXY=https://goproxy.githubapp.com/mod,https://proxy.golang.org/,direct
export GONOSUMDB='github.com/github/*'
export GONOPROXY=

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# Enable with line at beginning for profiling
# zprof
# XXX Don't add anything below this XXX
