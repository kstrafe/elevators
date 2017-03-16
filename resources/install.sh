#! /usr/bin/env bash

echo "Password: "
read pass
if ! [ -e racket-6.8-x86_64-linux.sh ]; then
	wget 'https://mirror.racket-lang.org/installers/6.8/racket-6.8-x86_64-linux.sh'
fi
chmod u+x racket-6.8*
echo $pass | sudo -S ./racket-6.8* <<EOF
yes
2


EOF
sudo raco pkg install --skip-installed lens libuuid reloadable sha threading <<< a
