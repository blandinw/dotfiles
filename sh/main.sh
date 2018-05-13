#!/usr/bin/env sh

source "$HOME/dotfiles/sh/z.sh"

set -o vi

# Bash-specific
if [ ! -z "$BASH_VERSION" ]; then
  shopt -s histappend
  export PS1='\n\[\033[0;35m\]\u@\h\[\033[00m\]:\w \nλ '
fi

export EDITOR='vim'
export TERM='xterm-256color'
export LANG='en_US.UTF-8'
export HISTSIZE=10000
export SAVEHIST=10000
export HISTFILE="$HOME/.histfile"
export JAVA_TOOL_OPTIONS='-Dfile.encoding=UTF8'
export PATH="$HOME/dotfiles/bin:$HOME/bin:/usr/local/bin:/usr/local/go/bin:/usr/local/sbin:$PATH"

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
alias hp='hg prev'
alias hn='hg next'
alias ht='hg top'
alias hb='hg bottom'
alias hgresolvetheirs='hg resolve -t internal:other --all'
alias hgresolveyours='hg resolve -t internal:local --all'
alias hgpullrebaseall="hg pull && hg rebase -d master -r '::bookmark() - ::master'"
hgfiles() {
  REV=$1
  if [ -z "$REV" ]; then
    REV=.
  fi
  hg log -r "$REV" --template "Added: {file_adds} Modified: {file_mods} Deleted: {file_dels}" | tr -s " " "\n"
}
hgswap() {
  book=__hgswap__
  hg book -r '.^' $book
  hg rebase -r '.' -d '.^^'
  hg rebase -r $book -d '.'
  hg book -d $book
}
hushe () {
  hg she && hg update "$1"
}
huuns () {
  hg update "$1" && hg uns
}
husheuns () {
  hg she && hg update "$1" && hg uns
}
hpshe () {
  hg she && hg prev
}
hpuns () {
  hg prev && hg uns
}
hpsheuns () {
  hg she && hg prev && hg uns
}
hnshe () {
  hg she && hg next
}
hnuns () {
  hg next && hg uns
}
hnsheuns () {
  hg she && hg next && hg uns
}
htshe () {
  hg she && hg top
}
htuns () {
  hg top && hg uns
}
htsheuns () {
  hg she && hg top && hg uns
}
hbshe () {
  hg she && hg bottom
}
hbuns () {
  hg bottom && hg uns
}
hbsheuns () {
  hg she && hg bottom && hg uns
}

# python
alias p='python'
alias json='python -m json.tool'
alias venv='source bin/activate'

uri () {
  node -e 'console.log(encodeURIComponent(`'$1'`))'
}


source "$HOME/dotfiles/sh/local.sh"

