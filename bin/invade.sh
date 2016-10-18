#!/usr/bin/env sh

set -ex

PREFIX="$HOME"
DOTFILES="$PREFIX/dotfiles"
BACKUPS="$PREFIX/backups"

sym () {
  SRC="$DOTFILES/$1"
  DEST="$PREFIX/$2"
  mkdir -p "$(dirname "$DEST")"

  if [ -e "$DEST" ]; then
    NEWDEST="$BACKUPS/$(basename "$DEST")-$(date +%s)"
    mv "$DEST" "$NEWDEST"
    echo "> Moved $DEST to $NEWDEST"
  fi

  ln -s "$SRC" "$DEST"
}

if [ ! -e "$DOTFILES" ]; then
  echo "error: dotfiles/ needs to reside in $PREFIX"
  exit 1
fi

mkdir -p "$PREFIX"
mkdir -p "$BACKUPS/vim_backups"

sym sh/bashrc      .bashrc
sym sh/zshrc       .zshrc
sym fish           .config/fish
sym ackrc          .ackrc
sym ctags          .ctags
sym tmux           .tmux
sym tmux/tmux.conf .tmux.conf
sym idea/ideavimrc .ideavimrc
sym kwmrc          .kwm/kwmrc

touch "$DOTFILES/sh/local.sh"
touch "$DOTFILES/fish/local.fish"

# ------------------------------------------------------------------------------
# Vim

sym vim            .vim
sym vim/vimrc      .vimrc
touch "$DOTFILES/vim/local.vim"
BUNDLE_DIR="$DOTFILES/vim/bundle"
if [ ! -e "$BUNDLE_DIR/Vundle.vim" ]; then
  git clone https://github.com/VundleVim/Vundle.vim.git "$BUNDLE_DIR"/Vundle.vim
fi

# ------------------------------------------------------------------------------
# IntelliJ Idea & CLion

for ide in IdeaIC15 clion11; do
  sym idea/idea.vmoptions  Library/Preferences/$ide/idea.vmoptions
  sym idea/idea.properties Library/Preferences/$ide/idea.properties
  sym idea/willy.icls Library/Preferences/$ide/colors/willy.icls
  sym idea/willy.xml Library/Preferences/$ide/keymaps/willy.xml
done

# ------------------------------------------------------------------------------
# Atom

sym atom/keymap.cson   .atom/keymap.cson
sym atom/snippets.cson .atom/snippets.cson

# ------------------------------------------------------------------------------
# Emacs + Spacemacs

if [ ! -d "$PREFIX/.emacs.d/layers" ]; then
  # backup existing emacs.d
  sym emacs.d .emacs.d
  # remove the symlink we just created
  rm "$PREFIX/.emacs.d"
  # install Spacemacs
  git clone https://github.com/syl20bnr/spacemacs "$PREFIX/.emacs.d"
fi
sym emacs.d/spacemacs .spacemacs

echo "> Invasion successful!"
