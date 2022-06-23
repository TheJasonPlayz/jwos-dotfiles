#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export PATH=$PATH:~/.emacs.d/bin/

alias ls='ls --color=auto'
alias emacs='emacs'
alias emacsc='emacsclient -c -a "emacs"'
alias em='emacs -nw'
alias emacsd='emacs --daemon'
alias recompx='xmonad --recompile && xmonad --restart'
PS1='[\u@\h \W]\$ '
