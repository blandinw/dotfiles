#!/usr/bin/env sh

# general
alias ls='ls -p -G'
alias ll='ls -l -h --color'
alias editlocal='vim $HOME/dotfiles/sh/local.sh; source $HOME/dotfiles/sh/bashrc'
alias dotfiles='vim $HOME/dotfiles'
alias t250='tail -n250'
alias xee='open -a xee'
alias vi='vim --noplugin'
alias hosts='sudo vim /etc/hosts'
alias tmuxw='tmux new -s w'
alias ed='emacs --daemon'
alias e='emacsclient -nw'
alias d=docker
alias a=atom

# git
alias g='git status'
alias gst='git status'
alias gf='git fetch'
alias gm='git merge'
alias ga='git add'
alias gb='git branch'
alias gc='git commit'
alias gck='git checkout'
alias gd='git diff'
alias gca='git commit -a'
alias gcaa='git commit -a --amend -C HEAD'
alias gg='git grep'
alias glo='git log'
alias gl='git pull'
alias gp='git push'

# mercurial
alias h='hg log -l1 ; hg status'
alias hc='hg commit'
alias ha='hg amend'
alias hdi='hg diff'
alias hlo='hg log'
alias hu='hg update'

# python
alias p='python'
alias json='python -m json.tool'
alias venv='source bin/activate'
