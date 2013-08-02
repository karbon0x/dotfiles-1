# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME='faust'
# Example aliases
# Aliases
## Typos
alias gi='git '
alias ir='irb '
alias pr='pry'
alias sbul='subl '
alias sbul.='subl .'
alias subl.='subl .'
## Unix
alias la='ls -alFGh'
alias ll='ls -lFGh'
alias lr='ls -lFGhR'
alias ls= 'ls -FG'
alias grep='grep --color=auto -n '
alias mkdir='mkdir -p '
## Ruby
alias b='bundle '
alias be='bundle exec '
alias gemset='rvm gemset list | grep "=" | awk "{print \$2}"'
#mysql
alias mysql='mysql -u root -p'
#redis
alias start_redis='redis-server /usr/local/etc/redis.conf'

source ~/.zshenv_private

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(brew bundler gem git knife mvn node npm osx rails ruby rvm)

source $ZSH/oh-my-zsh.sh
