#!/usr/bin/env sh

# shellcheck source=z.sh
. "$HOME/dotfiles/sh/z.sh"

set -o vi

# Bash-specific
# shellcheck disable=SC2039
if [ "$BASH_VERSION" ]; then
  shopt -s histappend
  export PS1='\n\[\033[0;35m\]\u@\h\[\033[00m\]:\w \nλ '
fi

export TERM=xterm-256color
export EDITOR='vim'
export LANG='en_US.UTF-8'
export HISTSIZE=10000
export SAVEHIST=10000
export HISTFILE="$HOME/.histfile"
export PATH="$HOME/dotfiles/bin:$HOME/bin:/usr/local/bin:/usr/local/go/bin:/usr/local/sbin:$PATH"

# general
alias ls='ls -p -G'
if [ "$(uname)" = Darwin ]; then
  alias ll='ls -lhG'
else
  alias ll='ls -l -h --color'
fi
alias vi='vim --noplugin'
alias hosts='sudo vim /etc/hosts'
alias tmuxw='tmux new -s w'
alias ed='emacs --daemon'
alias e='emacsclient'
alias d=docker

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
github() {
  echo "https://github.com/$(git config --get remote.origin.url | sed -E 's,.*github.com(:|/)([a-zA-Z0-9_-]+)/([a-zA-Z0-9_-]+)(\.git)?,\2/\3,')"
}

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
hushe() {
  hg shelve && hg update "$1"
}
huuns() {
  hg update "$1" && hg unshelve
}
husheuns() {
  hg shelve && hg update "$1" && hg unshelve
}
hpshe() {
  hg shelve && hg prev
}
hpuns() {
  hg prev && hg unshelve
}
hpsheuns() {
  hg shelve && hg prev && hg unshelve
}
hnshe() {
  hg shelve && hg next
}
hnuns() {
  hg next && hg unshelve
}
hnsheuns() {
  hg shelve && hg next && hg unshelve
}
htshe() {
  hg shelve && hg top
}
htuns() {
  hg top && hg unshelve
}
htsheuns() {
  hg shelve && hg top && hg unshelve
}
hbshe() {
  hg shelve && hg bottom
}
hbuns() {
  hg bottom && hg unshelve
}
hbsheuns() {
  hg shelve && hg bottom && hg unshelve
}

# python
alias p='python'
alias json='python -m json.tool'

# rust
export PATH="$HOME/.cargo/bin:$PATH"
if [ -f "$HOME/.cargo/env" ]; then
  . "$HOME/.cargo/env"
fi

# bat
if command -v bat >/dev/null; then
  alias cat=bat
fi

# ripgrep
export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"

# gdb
alias gdb='gdb -q'

c() {
  cd "$1" || return
  ll
}
dotfiles() {
    eval "$EDITOR $HOME/dotfiles"
}
dotsh() {
    eval "$EDITOR $HOME/dotfiles/sh/{main,local}.sh"
    source "$HOME/dotfiles/sh/bashrc"
}
rmpath() {
    PATH="$(echo "$PATH" | perl -anE 'chomp ; @arr = split ":" ; @arr = grep(!/'"$1"'/, @arr) ; say join(":", @arr)')"
    export PATH="$PATH"
    echo "PATH=$PATH"
}
uri() {
  node -e "console.log(encodeURIComponent(\`$1\`))"
}

. "$HOME/dotfiles/sh/local.sh"

