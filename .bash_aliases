# Verbosity and interactivity (prompts) for common commands
alias \
    cp='cp -iv' \
    mv='mv -iv' \
    rm='rm -Iv' \
    mkdir='mkdir -v' \
    rmdir='rmdir -v'

# ls and exa
alias \
    ls='ls     --color=auto --group-directories-first' \
    la='ls -A  --color=auto --group-directories-first' \
    ll='ls -lA --color=auto --group-directories-first --human-readable'
command -v exa >/dev/null \
    && alias \
        ls='exa     --group-directories-first --icons' \
        la='exa -a  --group-directories-first --icons' \
        ll='exa -la --group-directories-first --icons --grid --git' \
        lt='exa -Ta --group-directories-first --icons --level 2' \
        ltt='exa -Ta --group-directories-first --icons --level 3' \
        lttt='exa -Ta --group-directories-first --icons --level 4' \
        lT='exa -Ta --group-directories-first --icons' \
    || echo "exa not found"
        
# Grep
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# Set up dotfiles syncing
alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles-git/ --work-tree=$HOME'

# Check battery
alias battery='upower -i /org/freedesktop/UPower/devices/battery_BAT0 | grep -E "state|to\ full|percentage"'

# Other
alias tmux='tmux -2'                                    # assumes 256 colour

# Use neovim instead of vim if neovim is present
command -v nvim >/dev/null && alias vim='nvim' vimdiff="nvim -d" || echo "nvim not found"
