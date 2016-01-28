# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#ZSH_THEME="robbyrussell"
#ZSH_THEME="random"
ZSH_THEME="mrtazz"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

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

#export apt_pref='apt-get'
# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git ruby python zsh-history-substring-search nyan zsh-syntax-highlighting safe-paste)

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

export PATH=${HOME}/bin:/usr/local/bin:/opt/bin:/opt/local/bin:/opt/local/sbin:/bin:/usr/bin:/usr/local/sbin:/usr/games/bin

export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

unsetopt correctall

source ~/.zshalias

export RUBYLIB=''
export WORKDIR='/home/patrick/work'
# export FACTER_LOCATION="file://$WORKDIR/facter"
export PUPPET_LOCATION="file://$WORKDIR/puppet"

# Use local beaker
export RUBYLIB=${WORKDIR}/beaker/lib:${RUBYLIB}
export PATH=${PATH}:${WORKDIR}/beaker/bin

export RUST_SRC_PATH="${HOME}/src/rust/src"

# Fix stupid oh-my-zsh LSCOLORS
unset LSCOLORS

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
