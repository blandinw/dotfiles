#!/usr/bin/env sh

set -ex

PREFIX="$HOME"
DOTFILES="$PREFIX/dotfiles"
BACKUPS="$PREFIX/backups"

function sym () {
  src="$DOTFILES/$1"
  dest="$PREFIX/$2"
  mkdir -p "$(dirname $dest)"

  if [ -e "$dest" ]; then
    newdest="$BACKUPS/$(basename $dest)-$(date +%s)"
    mv "$dest" "$newdest"
    echo "> Moved $dest to $newdest"
  fi

  ln -s "$src" "$dest"
}

if [ ! -e "$DOTFILES" ]; then
  echo "error: dotfiles/ needs to reside in $PREFIX"
  exit 1
fi

mkdir -p "$PREFIX"
mkdir -p "$BACKUPS/vim_backups"

sym vim            .vim
sym vim/vimrc      .vimrc
sym bashrc         .bashrc
sym ackrc          .ackrc
sym ctags          .ctags
sym zshrc          .zshrc
sym emacs.d        .emacs.d
sym tmux           .tmux
sym tmux/tmux.conf .tmux.conf
sym idea/ideavimrc .ideavimrc

touch $DOTFILES/bash/local
touch $DOTFILES/vim/local.vim

# Vim
BUNDLE_DIR=$DOTFILES/vim/bundle
if [ ! -e $BUNDLE_DIR/Vundle.vim ]; then
  git clone https://github.com/VundleVim/Vundle.vim.git $BUNDLE_DIR/Vundle.vim
fi

# IntelliJ Idea & CLion
for ide in IdeaIC15 clion11; do
  sym idea/idea.vmoptions  Library/Preferences/$ide/idea.vmoptions
  sym idea/idea.properties Library/Preferences/$ide/idea.properties
  sym idea/willy.icls Library/Preferences/$ide/colors/willy.icls
  sym idea/willy.xml Library/Preferences/$ide/keymaps/willy.xml
done

# Atom
sym atom/keymap.cson   .atom/keymap.cson
sym atom/snippets.cson .atom/snippets.cson

echo "> Invasion successful!"
