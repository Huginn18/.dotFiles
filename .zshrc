export PATH=$HOME/bin/:$HOME/.local/bin/:$HOME/.config/bin/:$PATH

#LD_PRELOAD=/usr/lib/libcurl.4.so:/home/huginn/.config/bin/spotifywm.so /snap/bin/spotify

# === == = == === == = == ===
# Prompt
# === == = == === == = == ===
setopt PROMPT_SUBST
#autoload -Uz vcs_info
#precmd() { vcs_info }

#zstyle ':vcs_info:git:*' formats 'on branch %b'

PROMPT='${PWD/#$HOME/~}> '
#PROMPT='${PWD/#$HOME/~} %F{012}git: %F{004}${vcs_info_msg_0_}%F{015}> '

#git check-ignore -q . 2>/dev/null; if [ "$?" -ne "128" ]; then PROMPT='???'
# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history

# Use modern completion system
autoload -Uz compinit
compinit

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

# === == = == === == = == === == = == ===
# 		  Aliases
# === == = == === == = == === == = == ===
alias app-install="sudo apt install"
alias app-search="apt search"
alias app-update="sudo apt update"
alias dotfiles='/usr/bin/git --git-dir=$HOME/projects/.dotfiles/ --work-tree=$HOME'
alias music='/home/huginn/.config/bin/spotify.sh'
alias deb='sudo dpkg -i'

alias vpn='sudo surfshark-vpn'
alias vpn-q='sudo surfshark-vpn attack'
alias vpn-d='sudo surfshark-vpn down'
alias vpn-s='sudo surfshark-vpn status'

# === == = == === == = == === == = == ===
#                   GPG
# === == = == === == = == === == = == ===
unset SSH_AGENT_PID
     if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
       export SSH_AUTH_SOCK="${HOME}/.gnupg/S.gpg-agent.ssh"
     fi
