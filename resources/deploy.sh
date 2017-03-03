#! /usr/bin/env bash

echo -n "Enter address: "
read address
echo -n "Enter password: "
read pass
dir=sanntid-gruppe73
user=student

rsync -r -auv ../* $user@$address:~/"$dir"/
ssh $user@$address <<EOF
set +e
cd "$dir"
if ! [ -e racket-6.8-x86_64-linux.sh ]; then
	wget 'https://mirror.racket-lang.org/installers/6.8/racket-6.8-x86_64-linux.sh'
fi
chmod u+x racket-6.8*
echo $pass | sudo -S ./racket-6.8*
sudo raco pkg install --skip-installed lens libuuid reloadable sha threading <<< a
rm temporaries/*
# { nohup ./main.rkt & } &
./main.rkt >& temporaries/output-stream
EOF
