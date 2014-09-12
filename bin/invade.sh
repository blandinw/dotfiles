#!/usr/bin/env sh

DOTFILES="$HOME/dotfiles"
BACKUPS="$HOME/backups"
SYMLINKS='vim vim/vimrc bashrc ackrc ctags zshrc emacs.d'

if [ ! -e $DOTFILES ]; then
  echo "error: dotfiles/ needs to reside in $HOME"
  exit 1
fi

set -e
set -x

mkdir -p "$BACKUPS/vim_backups"

cd
for rcfile in $SYMLINKS; do
  base=$(basename $rcfile)
  timestamp=$(date +%s)

  if [ -e ".$base" ]; then
    dest="$BACKUPS/$base.$timestamp"
    mv ".$base" $dest
    echo "> Moved .$base to $dest"
  fi

  ln -s "$DOTFILES/$rcfile" ".$base"
done

# custom symlinks
ln -s "$DOTFILES/tmux" ~/.tmux
ln -s "$DOTFILES/tmux/tmux.conf" ~/.tmux.conf

touch dotfiles/bash/local
touch dotfiles/vim/local.vim

# Vim
BUNDLE_DIR=$DOTFILES/vim/bundle
if [ ! -e $BUNDLE_DIR/ctrlp.vim.git ]; then
  mkdir -p $BUNDLE_DIR
  git clone git://github.com/kien/ctrlp.vim.git $BUNDLE_DIR/ctrlp.vim.git
fi

set +x
echo "> Invasion successful!"
