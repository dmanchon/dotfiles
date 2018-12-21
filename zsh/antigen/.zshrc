LANG=en_US.UTF-8
LC_ALL=en_US.UTF-8

source /home/dmanchon/bin/antigen.zsh

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle git
antigen bundle pyenv
antigen bundle python
#antigen bundle pip
antigen bundle virtualenv
antigen bundle docker
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-history-substring-search
antigen bundle emacsclient
antigen bundle redis-cli
antigen bundle npm
antigen bundle node
antigen bundle golang
antigen bundle mvn
antigen bundle pass
antigen bundle sbt
antigen bundle vagrant
antigen bundle cp
antigen bundle bower
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

# Python
PYENV_ROOT="$HOME/.pyenv"
PATH="/home/dmanchon/go/bin:$PYENV_ROOT/bin:$PATH"
WORKON_HOME=~/.envs
eval "$(pyenv init -)"

# k8s
alias k='KUBE_EDITOR="emacsclient -c" kubectl'
autoload -U +X compinit && compinit
if [ $commands[kubectl] ]; then
  source <(kubectl completion zsh)
fi

# Aliases
alias magit='emacsclient -nw -c -e "(magit-status)"'

# Plan 9
unalias 9
PLAN9="/home/dmanchon/Projects/plan9"
PATH="$PATH:$PLAN9/bin"
alias rc='9 9term rc'

source ~/.opam/opam-init/init.zsh

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/dmanchon/.sdkman"
[[ -s "/home/dmanchon/.sdkman/bin/sdkman-init.sh" ]] && source "/home/dmanchon/.sdkman/bin/sdkman-init.sh"

source "/home/dmanchon/.sdkman/bin/sdkman-init.sh"
export _JAVA_AWT_WM_NONREPARENTING=1
