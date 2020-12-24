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
The **vimrc** file is a heavily modified, monolithic version of what
you'd find in the vimrcs directory of https://github.com/amix/vimrc .
The repository doesn't explicitly include a license, but it includes a
submodule which is GPL so I would imagine it falls under the GPL
license as well.

That would probably make this work GPL as well.

... I'm not a lawyer.
