#!/bin/bash

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
alias tmuxw='tmux new -s w'
alias d=docker
alias e=emacsclient
alias p=python
rmpath() (
  set -e
  PATH="$(echo "$PATH" | perl -anE 'chomp ; @arr = split ":" ; @arr = grep(!/'"$1"'/, @arr) ; say join(":", @arr)')"
  export PATH="$PATH"
  echo "PATH=$PATH"
)
uri() {
  node -e "console.log(encodeURIComponent(\`$1\`))"
}
geocode() {
  curl -sL "https://geocode.xyz/$(uri "$1")?json=1" | jq -r '@text "\(.latt),\(.longt)"'
}
csv() (
  set -ex

  file="$1"
  shift
  declare -a args

  if [ -z "$1" ]; then
    args=("--all")
  else
    args=("$@")
  fi

  if [[ "$file" =~ \.xls$ ]]; then
    base="$(basename "$file" .xls)"
    soffice --convert-to "xlsx:Calc MS Excel 2007 XML" "$file"
    xlsx2csv.py "${args[@]}" "$base.xlsx" > "$base.csv"
  elif [[ "$file" =~ \.xlsx$ ]]; then
    base="$(basename "$file" .xlsx)"
    xlsx2csv.py "${args[@]}" "$file" > "$base.csv"
  fi
)
notif() {
  osascript -e 'display notification "'"$1"'"'
}

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

# rust
export PATH="$HOME/.cargo/bin:$PATH"
if [ -f "$HOME/.cargo/env" ]; then
  . "$HOME/.cargo/env"
fi

# erlang
alias r3=rebar3
edoc_r3() {
  rebar3 edoc || true

  if [ -z "$1" ]; then
    fd --max-depth 2 . _build/docs
  else
    pushd "$(fd "$1" _build/docs | head -n1)"

    if [ -f rebar.config ]; then
      # remove edoc customizations (edown, etc.)
      sed -i.old -e '/{profiles/,/}\./d' rebar.config
      rebar3 edoc || true
      mv rebar.config.old rebar.config
    elif [ -f Makefile ]; then
      make edoc
    else
      popd
    fi

    [ -f doc/index.html ] && open doc/index.html
    [ -f README.md ] && marked README.md -o README.html && open README.html
    [ -f README.asciidoc ] && asciidoctor README.asciidoc && open README.html
  fi
}

edoc_mix() {
  DIR="$(fd --max-depth 1 "$1" deps)"
  cd "$DIR"
  if [ -f rebar.config ]; then
    edoc_r3 "$@"
  elif [ -f mix.exs ]; then
    popd
    mix hex.docs offline "$(basename "$DIR")"
  fi
}

edoc() (
  set -ex

  if [ -f rebar.config ]; then
    edoc_r3 "$@"
  elif [ -f mix.exs ]; then
    edoc_mix "$@"
  fi
)

# bat
if command -v bat >/dev/null; then
  alias cat=bat
fi

# ripgrep
export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"

# gdb
alias gdb='gdb -q'

. "$HOME/dotfiles/sh/local.sh"

