# Path to oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Look in ~/.oh-my-zsh/themes/
ZSH_THEME="mrtazz"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git ruby python history-substring-search nyan zsh-syntax-highlighting safe-paste zsh-completions)

source $ZSH/oh-my-zsh.sh

# bind UP and DOWN arrow keys
for keycode in '[' '0'; do
  bindkey "^[${keycode}A" history-substring-search-up
  bindkey "^[${keycode}B" history-substring-search-down
done
unset keycode

# bind P and N for EMACS mode
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down

# bind k and j for VI mode
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

# up and down arrows
bindkey "${terminfo[kcuu1]}" history-substring-search-up      # start typing + [Up-Arrow] - fuzzy find history forward
bindkey "${terminfo[kcud1]}" history-substring-search-down    # start typing + [Down-Arrow] - fuzzy find history backward

path=(~/.rbenv/bin $path)
eval "$(rbenv init -)"

unsetopt correctall

source ~/.zshalias

export WORKDIR='/home/patrick/work'
# export FACTER_LOCATION="file://$WORKDIR/facter"
# export PUPPET_LOCATION="file://$WORKDIR/puppet"

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

# added by travis gem
[ -f /home/patrick/.travis/travis.sh ] && source /home/patrick/.travis/travis.sh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

alias git=hub

export RIPGREP_CONFIG_PATH="${HOME}/.ripgreprc"
