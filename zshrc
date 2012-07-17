source ~/.bashrc

# settings
autoload -U colors && colors

# hack to get multiline prompt
precmd() {  print -rP "%{$fg[magenta]%}%n@%M:%{$reset_color%}%~" }
PROMPT='→ '
