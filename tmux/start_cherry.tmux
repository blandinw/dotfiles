# witd
rename-session cherry
renamew witd
splitw -v -p 20 -t 0
splitw -h -p 50 -t 1
selectp -t 0

setw synchronize-panes on
send -t 0 cdwitd ENTER
setw synchronize-panes off

send -t 0 "emacs -nw ." ENTER
send -t 1 "cargo run"

# cherry
neww -n cherry
splitw -v -p 20 -t 0
splitw -h -p 50 -t 1
selectp -t 0

setw synchronize-panes on
send -t 0 cdcherry ENTER
setw synchronize-panes off

send -t 0 "emacs -nw ." ENTER
send -t 1 "lein cljsbuild auto dev" ENTER
