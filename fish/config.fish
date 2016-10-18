function export_path
  set -gx $argv[1] (echo $argv[2] | tr ': ' \n)
end

source ~/dotfiles/sh/variables.sh
source ~/dotfiles/sh/aliases.sh

fish_vi_key_bindings
# set -U fish_term24bit 0
# set -U fish_term256 1
# set -U fish_color_user 5fafff --bold
# set -U fish_color_host 5fafff --bold
# set -U fish_escape_delay_ms 10

source ~/dotfiles/fish/local.fish

