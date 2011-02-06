#!/usr/bin/env sh

cd ~
mv .bashrc .bashrc.old
mv .zshrc .zshrc.old
mv .vimrc .vimrc.old
mv .ackrc .ackrc.old
mv .gvimrc .gvimrc.old
ln -s dotfiles/bashrc .bashrc
ln -s dotfiles/zshrc .zshrc
ln -s dotfiles/vim .vim
ln -s .vim/vimrc .vimrc
ln -s .vim/gvimrc .gvimrc
ln -s dotfiles/ackrc .ackrc

touch dotfiles/bash/local
