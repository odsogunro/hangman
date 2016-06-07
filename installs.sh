#! /usr/bin/env bash

##
# https://www.virtualbox.org/wiki/Linux_Downloads
# https://www.virtualbox.org/wiki/Downloads
##

# echo "deb http://download.virtualbox.org/virtualbox/debian xenial contrib" >> /etc/apt/sources.list

# wget -q https://www.virtualbox.org/download/oracle_vbox_2016.asc -O- | sudo apt-key add -
# wget -q https://www.virtualbox.org/download/oracle_vbox.asc -O- | sudo apt-key add -

# sudo apt-get update && sudo apt-get install -y virtualbox-5.0


##
# Replace virtualbox-5.0 by virtualbox-4.3 to install VirtualBox 4.3.36 Note: Ubuntu/Debian 
# users might want to install the dkms package to ensure that the VirtualBox host kernel 
# modules (vboxdrv, vboxnetflt and vboxnetadp) are properly updated if the linux kernel version 
# changes during the next apt-get upgrade. For Debian it is available in Lenny backports and 
# in the normal repository for Squeeze and later. The dkms package can be installed through 
# the Synaptic Package manager or through the following command:
##
# sudo apt-get install dkms

##
# What to do when experiencing The following signatures were invalid: BADSIG ... 
# when refreshing the packages from the repository?
## -----------------------------------------------------------------------------
# sudo -s -H
# apt-get clean
# rm /var/lib/apt/lists/*
# rm /var/lib/apt/lists/partial/*
# apt-get clean
# apt-get update

## 
# http://docs.haskellstack.org/en/stable/install_and_upgrade/#ubuntu
## 

sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442

echo 'deb http://download.fpcomplete.com/ubuntu xenial main'| sudo tee /etc/apt/sources.list.d/fpco.list

sudo apt-get update && sudo apt-get install -y stack

eval "$(stack --bash-completion-script stack)"