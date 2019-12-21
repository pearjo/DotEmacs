# My Emacs Configuration

## Install Configuration

To install the configuration and all fonts run either

```bash
./install.sh
```

on GNU/Linux system or

```batch
install.bat
```

if you're running Emacs on a Windows system.

## Emacs Daemon with systemd

By running the `install.sh` script, a systemd configuration file is
copied to `~/.config/systemd/user/`. To enable the unit run:

```bash
systemctl enable --user emacs
systemctl start --user emacs
```
