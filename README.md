# hangman

*vagrant: issues*
```
solutions: 
- https://www.virtualbox.org/wiki/Linux_Downloads
-- https://www.virtualbox.org/wiki/Downloads
          
==> default: Checking for guest additions in VM...
    default: The guest additions on this VM do not match the installed version of
    default: VirtualBox! In most cases this is fine, but in rare cases it can
    default: prevent things such as shared folders from working properly. If you see
    default: shared folder errors, please make sure the guest additions within the
    default: virtual machine match the version of VirtualBox you have installed on
    default: your host and reload your VM.
    default: 
    default: Guest Additions Version: 5.0.18_Ubuntu r106667
    default: VirtualBox Version: 4.3
==> default: Mounting shared folders...
    default: /home/hangman => /home/odkazeem/Projects/hangman
Failed to mount folders in Linux guest. This is usually because
the "vboxsf" file system is not available. Please verify that
the guest additions are properly installed in the guest and
can work properly. The command attempted was:

mount -t vboxsf -o uid=`id -u ubuntu`,gid=`getent group ubuntu | cut -d: -f3` home_hangman /home/hangman
mount -t vboxsf -o uid=`id -u ubuntu`,gid=`id -g ubuntu` home_hangman /home/hangman

The error output from the last command was:

sudo: unable to resolve host ubuntu-xenial
mesg: ttyname failed: Inappropriate ioctl for device
mount: unknown filesystem type 'vboxsf'

```
