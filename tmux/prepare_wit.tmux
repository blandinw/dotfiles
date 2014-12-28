# todo
rename-session wit
renamew todo
send -t 0 todo ENTER
splitw -v -p 20 -t 0
send -t 1 "hosts" ENTER

# wit
neww -n kernel
splitw -v -p 20 -t 0
splitw -h -p 50 -t 1
selectp -t 0

setw synchronize-panes on
send -t 0 cdkernel ENTER
setw synchronize-panes off

send -t 0 "emacs -nw ." ENTER
send -t 1 "lein do clean, repl" ENTER

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

# ops
neww -n wit-ops
splitw -v -p 10 -t 0
splitw -h -p 50 -t 0
selectp -t 0

setw synchronize-panes on
send -t 0 cdops ENTER
setw synchronize-panes off

send -t 2 routed ENTER
send -t 2 zip SPACE routed.nw SPACE package.json SPACE public/index.html SPACE public/js/cljs.js SPACE && SPACE sudo SPACE -E SPACE /Applications/node-webkit.app/Contents/MacOS/node-webkit SPACE --remote-debugging-port=9222 SPACE routed.nw ENTER

# squadron
neww -n squadron
splitw -v -p 20 -t 0
selectp -t 0

setw synchronize-panes on
send -t 0 cdwit ENTER
send -t 0 cd SPACE squadron ENTER
setw synchronize-panes off

send -t 1 "v reload" ENTER

# go back to wit-ops
select-window -t wit-ops
selectp -t 2
