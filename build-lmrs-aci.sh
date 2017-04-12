#/usr/bin/env bash

set -e
set -x

if [ -e out ]; then
    rm -rf out
fi
if [ -e aci-build/app ]; then
    rm -rf aci-build/app
fi
if [ -e aci-build/src ]; then
    rm -rf aci-build/src
fi

cp -r app aci-build/app
cp -r src aci-build/src
cp -r LICENSE aci-build/LICENSE

nix-env -i lmrs -f . -p out

# Start the build with an empty ACI
sudo -s acbuild --debug begin

acbuildEnd() {
    export EXIT=$?
    sudo -s acbuild --debug end && exit $EXIT 
}
trap acbuildEnd EXIT

for i in $(nix-store -qR out/); do sudo -s acbuild copy $i $i; done
#sudo -s acbuild copy $(realpath /etc/protocols) /etc/protocols
#for i in $(find /etc/ssl/certs); do sudo -s acbuild copy $(realpath $i) /etc/ssl/certs/$(basename $i); done
sudo -s acbuild set-exec $(realpath out/bin/lmrs-exe)
#sudo -s acbuild environment add PATH $(realpath out)/bin

sudo -s acbuild set-name toot.taxi/lmrs

sudo -s acbuild write --overwrite lmrs.aci

rm out
rm out-*-link
