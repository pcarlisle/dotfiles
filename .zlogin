# Set path in this file because codespaces wrongly overwrites it in /etc/zsh/zlogin

# Mark path as unique which makes it only keep the leftmost occurrence of an element
typeset -U path
path=(~/.zinit/polaris/bin ~/.emacs.d/bin ~/go/bin $path)
