#eval (python -m virtualfish auto_activation global_requirements)

function fish_prompt
    if not set -q VIRTUAL_ENV_DISABLE_PROMPT
        set -g VIRTUAL_ENV_DISABLE_PROMPT true
    end

    echo
    if test $VIRTUAL_ENV
        printf "%s" (set_color blue)(basename $VIRTUAL_ENV)(set_color normal)
    end
    set_color normal
    printf ' '
    set_color $fish_color_cwd
    printf '%s' (prompt_pwd)
    set_color normal

    # Line 2
    echo
    set_color yellow
    printf '$ '
    set_color normal
end

set PATH /usr/local/opt/node@10/bin /users/charlietanksley/bin /Library/TeX/texbin $HOME/.cargo/bin /usr/local/opt/nss/bin $PATH
source /usr/local/opt/asdf/asdf.fish
set -x PGHOST localhost
