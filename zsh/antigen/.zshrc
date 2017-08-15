LANG=en_US.UTF-8
LC_ALL=en_US.UTF-8
VIRTUALENVWRAPPER_PYTHON="/Users/dmanchon/.pyenv/shims/python"
PATH="$PATH:/usr/local/texlive/2017/bin/x86_64-darwin"
PATH="$PATH:/Users/dmanchon/Atlasense/google-cloud-sdk/bin"


source /usr/local/share/antigen/antigen.zsh

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle git
antigen bundle pyenv
antigen bundle python
antigen bundle pip
antigen bundle virtualenv
antigen bundle virtualenvwrapper
antigen bundle docker
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-history-substring-search
antigen bundle redis-cli
antigen bundle vundle
antigen bundle colored-man-pages
antigen bundle command-not-found
antigen bundle sroze/docker-compose-zsh-plugin
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-completions
antigen bundle tarruda/zsh-autosuggestions
antigen bundle osx
antigen bundle rupa/z
antigen bundle taskwarrior
antigen bundle chrissicool/zsh-256color
antigen bundle gpg-agent

# Load the theme.
antigen theme kennethreitz

# Tell Antigen that you're done.
antigen apply
antigen selfupdate

# Python
PYENV_ROOT="$HOME/.pyenv"
PATH="$PYENV_ROOT/bin:$PATH"
WORKON_HOME=~/.envs
eval "$(pyenv init -)"
alias brew="env PATH=${PATH//$(pyenv root)\/shims:/} brew"
disable r

archey -o
