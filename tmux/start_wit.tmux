# restart etcd and skydns
selectw -t squadron
send "vagrant ssh -c 'sudo systemctl restart etcd'"
send ENTER

# start kernel
selectw -t kernel
send -t 1 lein SPACE do SPACE clean, SPACE repl ENTER (-main) ENTER

# start corpora
selectw -t corpora
send -t 1 lein SPACE do SPACE clean, SPACE repl ENTER (-main) ENTER

# start turkd
selectw -t turkd
send -t 1 lein SPACE do SPACE clean, SPACE repl ENTER (-main) ENTER
