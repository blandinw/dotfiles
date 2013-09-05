source ~/.bashrc

# settings
autoload -U colors && colors
autoload -U compinit && compinit

export SAVEHIST=1000
export HISTSIZE=10000
export HISTFILE=~/.zsh_history
setopt inc_append_history
setopt share_history

# hack to get multiline prompt
precmd() {  print -rP "%{$fg[magenta]%}%n@%M:%{$reset_color%}%~" }
PROMPT='λ '

# bindings
bindkey '^R' 'history-incremental-search-backward'

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
