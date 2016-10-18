#!/usr/bin/env sh

# basics
export EDITOR='vim'
export TERM='xterm-256color'
export LANG='en_US.UTF-8'
export HISTSIZE=10000
export SAVEHIST=10000
export HISTFILE="$HOME/.histfile"
# export USERWM="$(which xmonad)"

# java
export JAVA_TOOL_OPTIONS='-Dfile.encoding=UTF8'

# allow Fish to execute own function
export_path PATH "$HOME/dotfiles/bin:$HOME/bin:/usr/local/bin:/usr/local/go/bin:/usr/local/sbin:$PATH"

