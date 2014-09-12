renamew cambridge
send -t 0 ./witd-arm ENTER

splitw -h
splitw -v -t 0
splitw -v -t 1

send -t 1 "cd cherry" ENTER "sudo node cherry.js config.json" ENTER
send -t 2 "cd spop" ENTER "./build_and_run -f" ENTER
send -t 3 "cowsay -f dragon 'cambridge started!'" ENTER