PATH=~/.cabal/bin:$PATH


alias trackoff="xinput set-prop 12 \"Device Enabled\" 0"
alias trackon="xinput set-prop 12 \"Device Enabled\" 1"
alias emax="emacsclient -c"

export PATH
export PS1="\[\e[33m\]\u@\[\e[m\]\w\\$ "

export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"
export VISUAL="emacsclient -c -a emacs"
export TERM=xterm-color
