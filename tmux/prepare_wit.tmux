# todo
rename-session wit
send -t 0 todo ENTER
splitw -v -p 20 -t 0

# wit
neww -n kernel
splitw -v -p 20 -t 0
splitw -h -p 50 -t 1
selectp -t 0

setw synchronize-panes on
send -t 0 cdkernel ENTER
setw synchronize-panes off

send -t 0 emacs SPACE -nw ENTER
# send -t 1 lein SPACE do SPACE clean, SPACE repl ENTER (-main) ENTER

# console
neww -n console
splitw -v -p 20 -t 0
splitw -h -p 50 -t 1
selectp -t 0

setw synchronize-panes on
send -t 0 cdconsole ENTER
setw synchronize-panes off

send -t 0 emacs SPACE -nw ENTER
send -t 2 grunt SPACE serve ENTER

# ops
neww -n wit-ops
splitw -h -p 50 -t 0
splitw -v -p 10 -t 0
selectp -t 0

setw synchronize-panes on
send -t 0 cdops ENTER
setw synchronize-panes off

send -t 0 v SPACE reload ENTER
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

# corpora
neww -n corpora
splitw -v -p 20 -t 0
selectp -t 0

setw synchronize-panes on
send -t 0 cdcorpora ENTER
setw synchronize-panes off

send -t 0 emacs SPACE -nw ENTER

# turkd
neww -n turkd
splitw -v -p 20 -t 0
selectp -t 0

setw synchronize-panes on
send -t 0 cdturkd ENTER
setw synchronize-panes off

send -t 0 emacs SPACE -nw ENTER
