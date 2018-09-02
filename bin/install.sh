#!/usr/bin/env sh
# shellcheck disable=SC1117,SC2059,SC2086,SC2145

set -e

PREFIX="$HOME"
DOTFILES="$PREFIX/dotfiles"
BACKUPS="$PREFIX/backups"
VENDOR="$DOTFILES/vendor"
DATE="$(date +%s)"

# ------------------------------------------------------------------------------
# Definitions

# https://stackoverflow.com/questions/4842424/list-of-ansi-color-escape-sequences
if command -v perl >/dev/null; then
  log() {
    printf ". $@" | perl -pe "s,$HOME(/|$),~/,g"
  }
else
  log() {
    printf ". $@"
  }
fi
normal() {
  printf "\033[0m$@"
}
color() {
  arg_sym='$@'
  eval "
  $1() {
    printf \"\033[$2m$arg_sym\" ; normal
  }
"
}
color blue  "38;5;45"
color bold  "1"
color gray  "38;5;241"
color green "38;5;82"
color red   "38;5;160"

sym() {
  SRC="$DOTFILES/$1"
  DEST="$PREFIX/$2"
  mkdir -p "$(dirname "$DEST")"
  moved=

  if [ -e "$DEST" ]; then
    NEWDEST="$BACKUPS/$(basename "$DEST")-$DATE"
    mv "$DEST" "$NEWDEST"
    moved=$(normal "(previous version moved to %s" "$(bold "$NEWDEST"))")
  fi

  log "linking %s to %s %s\n" "$(bold "$SRC")" "$(bold "$DEST")" "$moved"
  ln -s "$SRC" "$DEST"
}

check_path() {
  log "$1... "
  if command -v "$1" >/dev/null; then
    green 'found'
  else
    red 'not found'
  fi
  normal "\n"
}

git_clone() {
  log "cloning %s... " "$(bold "$1")"
  if [ -d "$2" ]; then
    blue "already exists"
  else
    git clone "$1" "$2" 2>/dev/null
    green "done"
  fi
  normal "\n"
}

# ------------------------------------------------------------------------------
# parse/validate

while [ $# -gt 0 ]; do
  case "$1" in
    --debug)
      shift
      DEBUG=true
      ;;
    *)
      shift
      ;;
  esac
done

if [ "$DEBUG" ]; then
  set -x
fi

if [ ! -e "$DOTFILES" ]; then
  echo "error: dotfiles/ needs to reside in $PREFIX"
  exit 1
fi

mkdir -p "$PREFIX"
mkdir -p "$VENDOR"

# ------------------------------------------------------------------------------
# git

check_path git
git config --global core.pager "diff-so-fancy | less --tabs=4 -RFX"
git config --bool --global diff-so-fancy.markEmptyLines false

# ------------------------------------------------------------------------------
# hg

check_path hg
if command -v hg >/dev/null; then
  gray "  use diff-so-fancy in Mercurial: hg config -e\n  [pager]\n  pager = diff-so-fancy | less --tabs=4 -RFX\n"
fi

# ------------------------------------------------------------------------------
# zsh

check_path zsh
touch "$DOTFILES/sh/local.sh"
sym sh/bashrc .bashrc
sym sh/zshrc .zshrc
git_clone https://github.com/zsh-users/zsh-completions "$VENDOR/zsh-completions"
git_clone https://github.com/zsh-users/zsh-autosuggestions "$VENDOR/zsh-autosuggestions"

# ------------------------------------------------------------------------------
# tmux

check_path tmux
sym tmux           .tmux
sym tmux/tmux.conf .tmux.conf

# ------------------------------------------------------------------------------
# ripgrep

check_path rg
sym ripgreprc .ripgreprc

# ------------------------------------------------------------------------------
# bat

check_path bat

# ------------------------------------------------------------------------------
# fd

check_path fd

# ------------------------------------------------------------------------------
# tldr

check_path tldr

# ------------------------------------------------------------------------------
# Vim

check_path vim
mkdir -p "$BACKUPS/vim_backups"
sym vim       .vim
sym vim/vimrc .vimrc
touch "$DOTFILES/vim/local.vim"
git_clone https://github.com/VundleVim/Vundle.vim.git "$DOTFILES/vim/bundle/Vundle.vim"

# ------------------------------------------------------------------------------
# Atom

check_path atom
sym atom/keymap.cson   .atom/keymap.cson
sym atom/snippets.cson .atom/snippets.cson

# ------------------------------------------------------------------------------
# Emacs + Spacemacs

check_path emacs
if [ ! -d "$PREFIX/.emacs.d/layers" ]; then
  # backup existing emacs.d
  sym emacs.d .emacs.d
  # remove the symlink we just created
  rm "$PREFIX/.emacs.d"
  # install Spacemacs
  git_clone https://github.com/syl20bnr/spacemacs "$PREFIX/.emacs.d"
fi
sym emacs.d/spacemacs .spacemacs

# ------------------------------------------------------------------------------
# IntelliJ Idea

sym idea/ideavimrc .ideavimrc
if [ "$(uname)" = "Darwin" ]; then
  for ide in IdeaIC15 clion11 IdeaIC2018.2; do
    sym idea/idea.vmoptions  Library/Preferences/$ide/idea.vmoptions
    sym idea/idea.properties Library/Preferences/$ide/idea.properties
    sym idea/willy.icls Library/Preferences/$ide/colors/willy.icls
    sym idea/willy.xml Library/Preferences/$ide/keymaps/willy.xml
  done
fi

echo "
[38;5;184m [39m[38;5;184m_[39m[38;5;178m_[39m[38;5;214m_[39m[38;5;214m_[39m[38;5;214m_[39m[38;5;214m_[39m[38;5;214m_[39m[38;5;214m_[39m[38;5;214m_[39m[38;5;214m_[39m[38;5;214m_[39m[38;5;208m_[39m[38;5;208m_[39m[38;5;208m_[39m[38;5;208m_[39m[38;5;208m_[39m[38;5;208m [39m[38;5;208m[39m
[38;5;214m<[39m[38;5;214m [39m[38;5;214mh[39m[38;5;214ma[39m[38;5;214mp[39m[38;5;214mp[39m[38;5;214my[39m[38;5;214m [39m[38;5;214mh[39m[38;5;208ma[39m[38;5;208mc[39m[38;5;208mk[39m[38;5;208mi[39m[38;5;208mn[39m[38;5;208mg[39m[38;5;208m~[39m[38;5;208m [39m[38;5;208m>[39m[38;5;208m[39m
[38;5;214m [39m[38;5;214m-[39m[38;5;214m-[39m[38;5;214m-[39m[38;5;214m-[39m[38;5;214m-[39m[38;5;208m-[39m[38;5;208m-[39m[38;5;208m-[39m[38;5;208m-[39m[38;5;208m-[39m[38;5;208m-[39m[38;5;208m-[39m[38;5;208m-[39m[38;5;208m-[39m[38;5;208m-[39m[38;5;209m-[39m[38;5;203m [39m[38;5;203m[39m
[38;5;214m [39m[38;5;214m [39m[38;5;214m [39m[38;5;208m [39m[38;5;208m [39m[38;5;208m [39m[38;5;208m\[39m[38;5;208m [39m[38;5;208m [39m[38;5;208m [39m[38;5;208m [39m[38;5;208m [39m[38;5;208m [39m[38;5;209m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;198m [39m[38;5;198m [39m[38;5;198m/[39m[38;5;198m [39m[38;5;198m\[39m[38;5;198m [39m[38;5;198m [39m[38;5;198m/[39m[38;5;198m/[39m[38;5;198m\[39m[38;5;199m[39m
[38;5;208m [39m[38;5;208m [39m[38;5;208m [39m[38;5;208m [39m[38;5;208m [39m[38;5;208m [39m[38;5;208m [39m[38;5;208m\[39m[38;5;208m [39m[38;5;208m [39m[38;5;209m [39m[38;5;203m [39m[38;5;203m|[39m[38;5;203m\[39m[38;5;203m_[39m[38;5;203m_[39m[38;5;203m_[39m[38;5;203m/[39m[38;5;203m|[39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;198m [39m[38;5;198m [39m[38;5;198m [39m[38;5;198m/[39m[38;5;198m [39m[38;5;198m [39m[38;5;198m [39m[38;5;198m\[39m[38;5;198m/[39m[38;5;198m/[39m[38;5;199m [39m[38;5;199m [39m[38;5;199m\[39m[38;5;199m\[39m[38;5;199m[39m
[38;5;208m [39m[38;5;208m [39m[38;5;208m [39m[38;5;208m [39m[38;5;208m [39m[38;5;208m [39m[38;5;208m [39m[38;5;209m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m/[39m[38;5;203m0[39m[38;5;203m [39m[38;5;203m [39m[38;5;203m0[39m[38;5;203m [39m[38;5;203m [39m[38;5;198m\[39m[38;5;198m_[39m[38;5;198m_[39m[38;5;198m [39m[38;5;198m [39m[38;5;198m/[39m[38;5;198m [39m[38;5;198m [39m[38;5;198m [39m[38;5;198m [39m[38;5;199m/[39m[38;5;199m/[39m[38;5;199m [39m[38;5;199m [39m[38;5;199m|[39m[38;5;199m [39m[38;5;199m\[39m[38;5;199m [39m[38;5;199m\[39m[38;5;163m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m[39m
[38;5;208m [39m[38;5;208m [39m[38;5;208m [39m[38;5;208m [39m[38;5;209m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m/[39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;198m [39m[38;5;198m/[39m[38;5;198m [39m[38;5;198m [39m[38;5;198m\[39m[38;5;198m/[39m[38;5;198m_[39m[38;5;198m/[39m[38;5;198m [39m[38;5;198m [39m[38;5;199m [39m[38;5;199m [39m[38;5;199m/[39m[38;5;199m/[39m[38;5;199m [39m[38;5;199m [39m[38;5;199m [39m[38;5;199m|[39m[38;5;199m [39m[38;5;163m [39m[38;5;164m\[39m[38;5;164m [39m[38;5;164m [39m[38;5;164m\[39m[38;5;164m [39m[38;5;164m [39m[38;5;164m[39m
[38;5;208m [39m[38;5;209m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m@[39m[38;5;203m_[39m[38;5;198m^[39m[38;5;198m_[39m[38;5;198m@[39m[38;5;198m'[39m[38;5;198m/[39m[38;5;198m [39m[38;5;198m [39m[38;5;198m [39m[38;5;198m\[39m[38;5;198m/[39m[38;5;199m_[39m[38;5;199m [39m[38;5;199m [39m[38;5;199m [39m[38;5;199m/[39m[38;5;199m/[39m[38;5;199m [39m[38;5;199m [39m[38;5;199m [39m[38;5;163m [39m[38;5;164m|[39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m\[39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m\[39m[38;5;164m [39m[38;5;164m[39m
[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;198m [39m[38;5;198m/[39m[38;5;198m/[39m[38;5;198m_[39m[38;5;198m^[39m[38;5;198m_[39m[38;5;198m/[39m[38;5;198m [39m[38;5;198m [39m[38;5;198m [39m[38;5;199m [39m[38;5;199m [39m[38;5;199m\[39m[38;5;199m/[39m[38;5;199m_[39m[38;5;199m [39m[38;5;199m/[39m[38;5;199m/[39m[38;5;199m [39m[38;5;163m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m|[39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m\[39m[38;5;164m [39m[38;5;164m [39m[38;5;128m [39m[38;5;129m [39m[38;5;129m\[39m[38;5;129m[39m
[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;198m [39m[38;5;198m([39m[38;5;198m [39m[38;5;198m/[39m[38;5;198m/[39m[38;5;198m)[39m[38;5;198m [39m[38;5;198m|[39m[38;5;198m [39m[38;5;198m [39m[38;5;199m [39m[38;5;199m [39m[38;5;199m [39m[38;5;199m [39m[38;5;199m [39m[38;5;199m [39m[38;5;199m\[39m[38;5;199m/[39m[38;5;199m/[39m[38;5;163m/[39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m|[39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;128m [39m[38;5;129m\[39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m\[39m[38;5;129m[39m
[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;203m [39m[38;5;198m [39m[38;5;198m [39m[38;5;198m([39m[38;5;198m [39m[38;5;198m/[39m[38;5;198m [39m[38;5;198m/[39m[38;5;198m)[39m[38;5;198m [39m[38;5;198m_[39m[38;5;199m|[39m[38;5;199m_[39m[38;5;199m [39m[38;5;199m/[39m[38;5;199m [39m[38;5;199m [39m[38;5;199m [39m[38;5;199m)[39m[38;5;199m [39m[38;5;163m [39m[38;5;164m/[39m[38;5;164m/[39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m|[39m[38;5;164m [39m[38;5;128m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m\[39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;93m [39m[38;5;93m_[39m[38;5;93m\[39m[38;5;93m[39m
[38;5;203m [39m[38;5;198m [39m[38;5;198m [39m[38;5;198m [39m[38;5;198m([39m[38;5;198m [39m[38;5;198m/[39m[38;5;198m/[39m[38;5;198m [39m[38;5;198m/[39m[38;5;198m)[39m[38;5;199m [39m[38;5;199m'[39m[38;5;199m/[39m[38;5;199m,[39m[38;5;199m_[39m[38;5;199m [39m[38;5;199m_[39m[38;5;199m [39m[38;5;199m_[39m[38;5;163m/[39m[38;5;164m [39m[38;5;164m [39m[38;5;164m([39m[38;5;164m [39m[38;5;164m;[39m[38;5;164m [39m[38;5;164m-[39m[38;5;164m.[39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;128m [39m[38;5;129m|[39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m_[39m[38;5;129m [39m[38;5;129m_[39m[38;5;129m\[39m[38;5;93m.[39m[38;5;93m-[39m[38;5;93m~[39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;63m [39m[38;5;63m.[39m[38;5;63m-[39m[38;5;63m~[39m[38;5;63m~[39m[38;5;63m~[39m[38;5;63m^[39m[38;5;63m-[39m[38;5;63m.[39m[38;5;63m[39m
[38;5;198m [39m[38;5;198m [39m[38;5;198m([39m[38;5;198m([39m[38;5;198m [39m[38;5;198m/[39m[38;5;198m [39m[38;5;198m/[39m[38;5;199m [39m[38;5;199m)[39m[38;5;199m)[39m[38;5;199m [39m[38;5;199m,[39m[38;5;199m-[39m[38;5;199m{[39m[38;5;199m [39m[38;5;199m [39m[38;5;163m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m_[39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;128m [39m[38;5;129m\`[39m[38;5;129m-[39m[38;5;129m.[39m[38;5;129m|[39m[38;5;129m.[39m[38;5;129m-[39m[38;5;129m~[39m[38;5;129m-[39m[38;5;129m.[39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;63m [39m[38;5;63m.[39m[38;5;63m~[39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;33m\`[39m[38;5;33m.[39m[38;5;33m[39m
[38;5;198m [39m[38;5;198m([39m[38;5;198m([39m[38;5;198m [39m[38;5;198m/[39m[38;5;199m/[39m[38;5;199m [39m[38;5;199m/[39m[38;5;199m [39m[38;5;199m)[39m[38;5;199m)[39m[38;5;199m [39m[38;5;199m [39m[38;5;199m'[39m[38;5;163m/[39m[38;5;164m\[39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m/[39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;128m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m~[39m[38;5;93m-[39m[38;5;93m.[39m[38;5;93m [39m[38;5;93m_[39m[38;5;93m [39m[38;5;63m.[39m[38;5;63m-[39m[38;5;63m~[39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m.[39m[38;5;63m-[39m[38;5;63m~[39m[38;5;33m^[39m[38;5;33m-[39m[38;5;33m.[39m[38;5;33m [39m[38;5;33m [39m[38;5;33m\[39m[38;5;33m[39m
[38;5;198m [39m[38;5;198m([39m[38;5;199m([39m[38;5;199m [39m[38;5;199m/[39m[38;5;199m/[39m[38;5;199m/[39m[38;5;199m [39m[38;5;199m)[39m[38;5;199m)[39m[38;5;199m [39m[38;5;163m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m\`[39m[38;5;164m.[39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m{[39m[38;5;164m [39m[38;5;128m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;93m [39m[38;5;93m}[39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m/[39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m\[39m[38;5;33m [39m[38;5;33m [39m[38;5;33m\[39m[38;5;39m[39m
[38;5;199m [39m[38;5;199m [39m[38;5;199m([39m[38;5;199m([39m[38;5;199m [39m[38;5;199m/[39m[38;5;199m [39m[38;5;199m)[39m[38;5;163m)[39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m.[39m[38;5;164m-[39m[38;5;164m-[39m[38;5;164m-[39m[38;5;164m-[39m[38;5;164m~[39m[38;5;128m-[39m[38;5;129m.[39m[38;5;129m\[39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;93m [39m[38;5;93m\[39m[38;5;93m-[39m[38;5;93m'[39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m.[39m[38;5;33m~[39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;39m\[39m[38;5;39m [39m[38;5;39m [39m[38;5;39m\`[39m[38;5;39m.[39m[38;5;39m [39m[38;5;39m\[39m[38;5;39m^[39m[38;5;39m-[39m[38;5;38m.[39m[38;5;44m[39m
[38;5;199m [39m[38;5;199m [39m[38;5;199m [39m[38;5;199m [39m[38;5;199m [39m[38;5;163m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m/[39m[38;5;164m/[39m[38;5;164m/[39m[38;5;164m.[39m[38;5;128m-[39m[38;5;129m-[39m[38;5;129m-[39m[38;5;129m-[39m[38;5;129m.[39m[38;5;129m.[39m[38;5;129m>[39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m\[39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m_[39m[38;5;63m [39m[38;5;63m-[39m[38;5;33m~[39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;39m [39m[38;5;39m [39m[38;5;39m [39m[38;5;39m [39m[38;5;39m\`[39m[38;5;39m.[39m[38;5;39m [39m[38;5;39m [39m[38;5;39m^[39m[38;5;38m-[39m[38;5;44m\`[39m[38;5;44m [39m[38;5;44m [39m[38;5;44m^[39m[38;5;44m-[39m[38;5;44m_[39m[38;5;44m[39m
[38;5;199m [39m[38;5;199m [39m[38;5;163m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;128m [39m[38;5;129m/[39m[38;5;129m/[39m[38;5;129m/[39m[38;5;129m-[39m[38;5;129m.[39m[38;5;129m_[39m[38;5;129m [39m[38;5;129m_[39m[38;5;129m [39m[38;5;93m_[39m[38;5;93m [39m[38;5;93m_[39m[38;5;93m [39m[38;5;93m_[39m[38;5;93m [39m[38;5;93m_[39m[38;5;93m [39m[38;5;93m_[39m[38;5;93m}[39m[38;5;63m^[39m[38;5;63m [39m[38;5;63m-[39m[38;5;63m [39m[38;5;63m-[39m[38;5;63m [39m[38;5;63m-[39m[38;5;63m [39m[38;5;63m-[39m[38;5;63m [39m[38;5;63m~[39m[38;5;63m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;39m [39m[38;5;39m [39m[38;5;39m [39m[38;5;39m [39m[38;5;39m [39m[38;5;39m [39m[38;5;39m [39m[38;5;39m [39m[38;5;39m [39m[38;5;38m [39m[38;5;44m~[39m[38;5;44m-[39m[38;5;44m-[39m[38;5;44m [39m[38;5;44m,[39m[38;5;44m.[39m[38;5;44m-[39m[38;5;44m~[39m[38;5;44m[39m
[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;164m [39m[38;5;128m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;129m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;93m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;63m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;33m [39m[38;5;39m [39m[38;5;39m [39m[38;5;39m [39m[38;5;39m [39m[38;5;39m [39m[38;5;39m [39m[38;5;39m [39m[38;5;39m [39m[38;5;39m [39m[38;5;38m [39m[38;5;44m [39m[38;5;44m [39m[38;5;44m [39m[38;5;44m/[39m[38;5;44m.[39m[38;5;44m-[39m[38;5;44m~[39m[38;5;44m[39m
"
