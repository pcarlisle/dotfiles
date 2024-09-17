# Set path in this file because codespaces wrongly overwrites it in /etc/zsh/zlogin

# Mark path as unique which makes it only keep the leftmost occurrence of an element
typeset -U path
path=(~/.zinit/polaris/bin ~/.config/emacs/bin ~/.emacs.d/bin ~/go/bin /opt/homebrew/bin $path)

# Added by OrbStack: command-line tools and integration
source ~/.orbstack/shell/init.zsh 2>/dev/null || :
