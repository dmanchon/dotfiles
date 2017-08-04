# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded. (default= robbyrussell)
if [ -n "$INSIDE_EMACS" ]; then
    export ZSH_THEME="rawsyntax"
else
    export ZSH_THEME="mh"
fi

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
alias emacsclient="emacsclient -nw -c -a emacs"
#alias atom="reattach-to-user-namespace atom"
autoload -Uz compinit bashcompinit

# Allow R to execute, i dont want the repeat shit
disable r

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git atom bower cp heroku mvn node npm osx pip python tmux vagrant brew wd sbt github scala svn virtualenv z docker lein pass pyenv themes)

# And load oh-my-zsh
source $ZSH/oh-my-zsh.sh

# Enable zsh syntax highlighting
source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
unset VIRTUAL_ENV_DISABLE_PROMPT

# Enable z
. `brew --prefix`/etc/profile.d/z.sh
alias brew="env PATH=${PATH//$(pyenv root)\/shims:/} brew"

# Print archey logo
archey -o

autoload -Uz compinit bashcompinit
compinit
bashcompinit


fpath=(/usr/local/share/zsh-completions $fpath)
source /usr/local/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/local/share/zsh-navigation-tools/zsh-navigation-tools.plugin.zsh
