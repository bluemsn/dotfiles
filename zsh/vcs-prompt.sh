# Git.
ZSH_VCS_PROMPT_GIT_FORMATS=' (%s)[%b%c%d|%e%f%g%h%i%j]'
ZSH_VCS_PROMPT_GIT_ACTION_FORMATS=' (%s)[%b:%a%c%d|%e%f%g%h%i%j]'
# Other vcs.
ZSH_VCS_PROMPT_VCS_FORMATS=' (%s)[%b]'
ZSH_VCS_PROMPT_VCS_ACTION_FORMATS=' (%s)[%b:%a]'

### Git.
## No action.
# VCS name
ZSH_VCS_PROMPT_GIT_FORMATS=' (%{%B%F{green}%}#s%{%f%b%})'
# Branch name
ZSH_VCS_PROMPT_GIT_FORMATS+='[%{%B%F{red}%}#b%{%f%b%}'
# Ahead and Behind
ZSH_VCS_PROMPT_GIT_FORMATS+='#c#d|'
# Staged
ZSH_VCS_PROMPT_GIT_FORMATS+='%{%F{blue}%}#e%{%f%b%}'
# Conflicts
ZSH_VCS_PROMPT_GIT_FORMATS+='%{%F{red}%}#f%{%f%b%}'
# Unstaged
ZSH_VCS_PROMPT_GIT_FORMATS+='%{%F{yellow}%}#g%{%f%b%}'
# Untracked
ZSH_VCS_PROMPT_GIT_FORMATS+='#h'
# Stashed
ZSH_VCS_PROMPT_GIT_FORMATS+='%{%F{cyan}%}#i%{%f%b%}'
# Clean
ZSH_VCS_PROMPT_GIT_FORMATS+='%{%F{green}%}#j%{%f%b%}]'


## No action using python.
# VCS name
ZSH_VCS_PROMPT_GIT_FORMATS_USING_PYTHON=' (%{%B%F{yellow}%}#s%{%f%b%})'
# Branch name
ZSH_VCS_PROMPT_GIT_FORMATS_USING_PYTHON+='[%{%B%F{red}%}#b%{%f%b%}'
# Ahead and Behind
ZSH_VCS_PROMPT_GIT_FORMATS_USING_PYTHON+='#c#d|'
# Staged
ZSH_VCS_PROMPT_GIT_FORMATS_USING_PYTHON+='%{%F{blue}%}#e%{%f%b%}'
# Conflicts
ZSH_VCS_PROMPT_GIT_FORMATS_USING_PYTHON+='%{%F{red}%}#f%{%f%b%}'
# Unstaged
ZSH_VCS_PROMPT_GIT_FORMATS_USING_PYTHON+='%{%F{yellow}%}#g%{%f%b%}'
# Untracked
ZSH_VCS_PROMPT_GIT_FORMATS_USING_PYTHON+='#h'
# Stashed
ZSH_VCS_PROMPT_GIT_FORMATS_USING_PYTHON+='%{%F{cyan}%}#i%{%f%b%}'
# Clean
ZSH_VCS_PROMPT_GIT_FORMATS_USING_PYTHON+='%{%F{green}%}#j%{%f%b%}]'


## Action.
# VCS name
ZSH_VCS_PROMPT_GIT_ACTION_FORMATS=' (%{%B%F{green}%}#s%{%f%b%})'
# Branch name
ZSH_VCS_PROMPT_GIT_ACTION_FORMATS+='[%{%B%F{red}%}#b%{%f%b%}'
# Action
ZSH_VCS_PROMPT_GIT_ACTION_FORMATS+=':%{%B%F{red}%}#a%{%f%b%}'
# Ahead and Behind
ZSH_VCS_PROMPT_GIT_ACTION_FORMATS+='#c#d|'
# Staged
ZSH_VCS_PROMPT_GIT_ACTION_FORMATS+='%{%F{blue}%}#e%{%f%}'
# Conflicts
ZSH_VCS_PROMPT_GIT_ACTION_FORMATS+='%{%F{red}%}#f%{%f%}'
# Unstaged
ZSH_VCS_PROMPT_GIT_ACTION_FORMATS+='%{%F{yellow}%}#g%{%f%}'
# Untracked
ZSH_VCS_PROMPT_GIT_ACTION_FORMATS+='#h'
# Stashed
ZSH_VCS_PROMPT_GIT_ACTION_FORMATS+='%{%F{cyan}%}#i%{%f%}'
# Clean
ZSH_VCS_PROMPT_GIT_ACTION_FORMATS+='%{%F{green}%}#j%{%f%}]'


### Other vcs.
## No action.
# VCS name
ZSH_VCS_PROMPT_VCS_FORMATS=' (%{%B%F{green}%}#s%{%f%b%})'
# Branch name
ZSH_VCS_PROMPT_VCS_FORMATS+='[%{%B%F{red}%}#b%{%f%b%}]'

## Action.
# VCS name
ZSH_VCS_PROMPT_VCS_ACTION_FORMATS=' (%{%B%F{green}%}#s%{%f%b%})'
# Branch name
ZSH_VCS_PROMPT_VCS_ACTION_FORMATS+='[%{%B%F{red}%}#b%{%f%b%}'
# Action
ZSH_VCS_PROMPT_VCS_ACTION_FORMATS+=':%{%B%F{red}%}#a%{%f%b%}]'

## The symbols.
ZSH_VCS_PROMPT_AHEAD_SIGIL='a'
ZSH_VCS_PROMPT_BEHIND_SIGIL='b'
ZSH_VCS_PROMPT_STAGED_SIGIL='*'
ZSH_VCS_PROMPT_CONFLICTS_SIGIL='x'
ZSH_VCS_PROMPT_UNSTAGED_SIGIL='+'
ZSH_VCS_PROMPT_UNTRACKED_SIGIL='.'
ZSH_VCS_PROMPT_STASHED_SIGIL='$'
ZSH_VCS_PROMPT_CLEAN_SIGIL='OK'
