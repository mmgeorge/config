#!/bin/bash

PROMPT_DIRTRIM=4

KILLEMACS="(kill-emacs)"

# bindings
alias clear='clear;tmux clear-history'
alias ll='ls -l --block-size=M --group-directories-first --color'
alias ls='ls --block-size=M --group-directories-first --color' 
alias lla='ls -la --block-size=M --group-directories-first --color'
alias ec='emacsclient -nw '
alias kec='pkill emacs'
alias create='$HOME/config/scripts/create-repo.sh'

alias wg='watch -n 1 gcloud'
alias wk='watch -n 1 kubectl'
alias k=kubectl
alias mk=kubectl
alias g=gcloud

eval "$(stack --bash-completion-script stack)"
source /etc/bash_completion.d/git-prompt  # enable git autocomplete
source <(kubectl completion bash) # enable autocompletion

complete -F __start_kubectl k # enable autocomplete for alias
complete -F __start_gcloud g # enable autocomplete for alias
complete -F __start_kubectl mk # enable autocomplete for alias
complete -F __start_kubectl wk # enable autocomplete for alias
complete -F __start_gcloud wg # enable autocomplete for alias

export PATH=$PATH:$HOME/.cargo/bin

# color directories
LS_COLORS=$LS_COLORS:'di=1;37:' ; export LS_COLORS


# -----------------------------------------------------------
# END MODIFICATIONS

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

export PS1="\[\033[38;5;11m\]\u\[$(tput sgr0)\]\[\033[38;5;15m\]@\h:\[$(tput sgr0)\]\[\033[38;5;6m\][\w]:\[$(tput sgr0)\]\[\033[38;5;15m\] \[$(tput sgr0)\]"

# # colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# # Add an "alert" alias for long running commands.  Use like so:
# #   sleep 10; alert
# alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(h#istory|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'


# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

<<<<<<< HEAD
EDITOR=emacs
=======
export GHCRTS='-M1G'
>>>>>>> ee9a59bb28bef52ea24acbb683d6f0469aa10b1a
