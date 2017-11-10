# Aliases and Functions

# Sometimes you want to see *everything*
alias glga="git log --graph --all"

# Make tux give fortunes if available
if hash cowsay 2>/dev/null; then
    if hash fortune 2>/dev/null; then
        alias tux-say='fortune -a -e | cowsay -f tux'
    else
        alias tux-say='echo "fortune not installed!"'
    fi
else
    alias tux-say='echo "cowsay not installed!"'
fi

# Exuberant ctags with tags for function declarations and definitions in C
CTAGS_EXECUTABLE=$(which ctags)
alias exutags="$CTAGS_EXECUTABLE --languages=C,C++ --c-kinds=+p  --fields=+iaS --extra=+q --recurse"

# Ctags for python
alias pytags="$CTAGS_EXECUTABLE --python-kinds=-i --recurse"

# GNU ARM Toolchain GDB alias
alias armgdb='arm-none-eabi-gdb -iex "add-auto-load-safe-path .gdbinit"'

# Shortcut for colormake
function cake() {
    if hash colormake 2>/dev/null; then
        if [ "$1" = "debug" ]; then
            shift;
            colormake "$@" && make debug "$@";
        elif [ "$1" = "nuke" ]; then
            colormake distclean && colormake annihilate-submodules;
        else colormake "$@";
        fi
    else
        echo "You don't have colormake.";
        make "$@";
    fi
}

alias cppgrep='grep --include="*.h" --include="*.cpp" --include="*.c" --include="*.cc" --include="*.cxx" '
alias edit=""$EDITOR""
alias vimclient="gvim --remote "
alias ec="emacsclient"

export MAKEFLAGS=-j`nproc`
