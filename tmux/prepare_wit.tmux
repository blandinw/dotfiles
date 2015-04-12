# todo
rename-session wit
renamew todo
send -t 0 todo ENTER
splitw -v -p 20 -t 0
send -t 1 "hosts" ENTER

# console
neww -n console
splitw -v -p 20 -t 0
splitw -h -p 50 -t 1
selectp -t 0

setw synchronize-panes on
send -t 0 cdconsole ENTER
setw synchronize-panes off

send -t 0 "emacs -nw ." ENTER
send -t 2 grunt SPACE serve ENTER

# kernel
neww -n kernel
splitw -v -p 20 -t 0
splitw -h -p 50 -t 1
selectp -t 0

setw synchronize-panes on
send -t 0 cdkernel ENTER
setw synchronize-panes off

send -t 0 "emacs -nw ." ENTER
send -t 1 "lein do clean, repl" ENTER

# corpora
neww -n corpora
splitw -v -p 20 -t 0
splitw -h -p 50 -t 1
selectp -t 0

setw synchronize-panes on
send -t 0 cdcorpora ENTER
setw synchronize-panes off

send -t 0 "emacs -nw ." ENTER
send -t 1 "lein do clean, repl" ENTER

# walt
neww -n walt
splitw -v -p 20 -t 0
splitw -h -p 50 -t 1
selectp -t 0

setw synchronize-panes on
send -t 0 cdwalt ENTER
setw synchronize-panes off

send -t 0 "emacs -nw ." ENTER
send -t 1 "lein do clean, repl" ENTER

# wit-ops
neww -n wit-ops
splitw -v -p 20 -t 0
selectp -t 0

setw synchronize-panes on
send -t 0 cdops ENTER
setw synchronize-panes off

# squadron
neww -n squadron
splitw -v -p 20 -t 0
selectp -t 0

setw synchronize-panes on
send -t 0 cdsquadron ENTER
setw synchronize-panes off

send -t 0 "v reload" ENTER
send -t 1 startproxy ENTER
