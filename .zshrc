# Enable for profiling (and zprof line at end)
# zmodload zsh/zprof

### Added by Zinit's installer
if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma/zinit%F{220})…%f"
    command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
    command git clone https://github.com/zdharma/zinit "$HOME/.zinit/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
            print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

source "$HOME/.zinit/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
# zinit light-mode for \
#       zinit-zsh/z-a-rust \
#       zinit-zsh/z-a-as-monitor \
#       zinit-zsh/z-a-patch-dl \
#       zinit-zsh/z-a-bin-gem-node

### End of Zinit's installer chunk

# zinit snippet OMZ::themes/mrtazz.zsh-theme

# Look in ~/.oh-my-zsh/themes/
# ZSH_THEME="mrtazz"

# plugins can be found in ~/.oh-my-zsh/plugins/*
# custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# plugins=(
# copydir
# copyfile
# gitfast
# github
# lein
# jsontools
# httpie
# golang
# python
# ruby
# rbenv
# history-substring-search
# safe-paste
# vagrant
# vault
# zsh_reload
# )

# ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=10'

## Theme
autoload colors
colors
setopt promptsubst
zinit wait lucid for \
        OMZL::git.zsh \
        OMZL::spectrum.zsh \
  atload"unalias grv" \
        OMZP::git
PS1="READY >" # provide a simple prompt till the theme loads
zinit wait'!' lucid for \
    OMZL::prompt_info_functions.zsh \
    OMZT::mrtazz

## Plugins

zinit light zsh-users/zsh-history-substring-search
# bind P and N for EMACS mode
# bindkey -M emacs '^P' history-substring-search-up
# bindkey -M emacs '^N' history-substring-search-down

# bind k and j for VI mode
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down
# up and down arrows
bindkey "${terminfo[kcuu1]}" history-substring-search-up
bindkey "${terminfo[kcud1]}" history-substring-search-down

zinit wait lucid for \
 atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" \
    zdharma/fast-syntax-highlighting \
 blockf \
    zsh-users/zsh-completions

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

zinit ice wait lucid
zinit light htlsne/zinit-rbenv

## History

if [ -z "$HISTFILE" ]; then
    HISTFILE=$HOME/.zsh_history
fi

HISTSIZE=100000
SAVEHIST=100000

## Shell options

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

alias l='ls -lah'
alias ll='ls -lh'
alias la='ls -lAh'
source ~/.zshalias

export WORKDIR='/home/patrick/work'
# export FACTER_LOCATION="file://$WORKDIR/facter"
# export PUPPET_LOCATION="file://$WORKDIR/puppet"
export GEM_SOURCE='https://rubygems.org'

# export RUBYLIB=''
# Use local beaker
# export RUBYLIB=${WORKDIR}/beaker/lib:${RUBYLIB}
# export PATH=${PATH}:${WORKDIR}/beaker/bin

export RUST_SRC_PATH="${HOME}/src/rust/src"

export JAVA_HOME=/usr/lib/jvm/default
export FLAMEGRAPH_DIR="${HOME}/work/FlameGraph"

# Set up nice ls colors
eval "$(dircolors .dircolors)"
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

PAGER=less;  	export PAGER

# Less options:
# -i : smart case in searches
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

# source $ZSH/lib/edit-command-line.zsh

export VAGRANT_INSTALLER_ENV=1

export TERM=xterm-256color

# [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export RIPGREP_CONFIG_PATH="${HOME}/.ripgreprc"

# autoload -U +X bashcompinit && bashcompinit
# complete -o nospace -C /usr/bin/vault vault


# eval "$(rbenv init -)"

# Enable with line at beginning for profiling
# zprof
