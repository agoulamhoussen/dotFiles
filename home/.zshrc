export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="alnour"

plugins=(git mvn virtualenvwrapper)

source $ZSH/oh-my-zsh.sh

[[ -f ~/Projects/home-env.sh ]] && . ~/Projects/home-env.sh
[[ -f ~/libon/libon-env.sh ]] && . ~/libon/libon-env.sh

