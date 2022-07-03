What you're getting
------------------------------

This is my configuration for emacs, zsh, and vim. For zsh it uses the grml
configuration, and it uses vim-pathogen for managing vim plugins.

Learn more about pathogen at https://github.com/tpope/vim-pathogen

Installation
------------------------------
Clone the dotfiles repository into ~/.dotfiles :

    git clone --recursive git@github.com:Josaphat/dotfiles.git ~/.dotfiles

Then make ~/.vimrc ~/.vim and ~/.zshrc into symbolic links to the
versions in the repository:

    ln -s ~/.dotfiles/zshrc ~/.zshrc
    ln -s ~/.dotfiles/zshrc.local ~/.zshrc.local
    ln -s ~/.dotfiles/vimrc ~/.vimrc
    ln -s ~/.dotfiles/vim/ ~/.vim

If you have an older git version or you forgot to use `--recursive` in
the clone step, you can run this after the fact:

    git submodule update --init --recursive

Customization
------------------------------
Generally you won't want to change the grml-provided `zshrc` file, and instead
you'll want to make changes to `zshrc.local`.

It used to be that I had a whole system for maintaining customizations separate
from "core" files, but meh. I found it was more effort than it's really worth
when you're running the same config on diverse systems (Fedora, FreeBSD, cygwin,
Arch, Ubuntu...), all subtly different.  Plus, it's updated so rarely I can't be
bothered.

Problems
------------------------------
This setup should be pretty straightforward to get going, but if you
run into problems submit an issue or e-mail me your questions ( jos at
josaphat dot co ).  Feel free to make improvements and send pull
requests.

License
------------------------------
Since this repository is an aggregate of many components, it doesn't
make much sense to apply a license to the whole thing.  It makes more
sense for licensing to be on a file-by-file basis. So, if a file
mentions a license, then that file is licensed as
indicated. Otherwise, if a file makes no mention of a license, then
that file is licensed under the Apache License, Version 2.0.

The repository as a whole is licensed under GPLv3.

Basically my intent is this: If you take an individual file, that file
is licensed either as indicated in the file, or if no license is
noted, then that file is licensed under the permissive Apache 2.0
license.

However, since there are some files licensed under GPL, then the work
as a whole (if you can even really call it that) must also be GPL.

Essentially, you're free to use this stuff however you want for your
development environment. But if you're distributing it somehow beyond
your own environments, then consider the licensing requirements.
