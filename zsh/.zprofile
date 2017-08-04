# User configuration
PATH="/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin"
PATH="$PATH:/usr/local/texlive/2015basic/bin/x86_64-darwin"
PATH="$PATH:/Users/dmanchon/Atlasense/google-cloud-sdk/bin"
PATH="/usr/local/opt/gpg-agent/bin:$PATH"

PYENV_ROOT="$HOME/.pyenv"
PATH="$PYENV_ROOT/bin:$PATH"
WORKON_HOME=~/.envs
eval "$(pyenv init -)"

# You may need to manually set your language environment
LC_ALL=en_US.UTF-8
LANG=en_US.UTF-8

if [ -f ~/.gnupg/.gpg-agent-info ] && [ -n "$(pgrep gpg-agent)" ]; then
    source ~/.gnupg/.gpg-agent-info
    export GPG_AGENT_INFO
else
    eval $(gpg-agent --daemon --write-env-file ~/.gnupg/.gpg-agent-info)
fi
