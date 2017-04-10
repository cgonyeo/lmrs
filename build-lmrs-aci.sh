#/usr/bin/env bash

set -e
set -x

if [ -e out ]; then
    rm -rf out
fi

nix-env -i lmrs -f . -p out
nix-env -i nss-cacert -p out
nix-env -i iana-etc -p out

# Start the build with an empty ACI
sudo -s acbuild --debug begin

acbuildEnd() {
    export EXIT=$?
    sudo -s acbuild --debug end && exit $EXIT 
}
trap acbuildEnd EXIT

for i in $(nix-store -qR out/); do sudo -s acbuild copy $i $i; done
sudo -s acbuild copy $(realpath /etc/protocols) /etc/protocols
for i in $(find /etc/ssl/certs); do sudo -s acbuild copy $(realpath $i) /etc/ssl/certs/$(basename $i); done
sudo -s acbuild set-exec $(realpath out/bin/lmrs-exe)
sudo -s acbuild environment add PATH $(realpath out)/bin

sudo -s acbuild set-name smuggle.rs/lmrs

sudo -s acbuild write --overwrite lmrs.aci
