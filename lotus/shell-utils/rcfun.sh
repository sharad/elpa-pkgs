# -*- Mode: shell-script; indent-tabs-mode: nil; sh-shell: zsh; -*-


##{{ emacs
# from: https://jpace.wordpress.com/2016/12/01/current-file-and-directory-in-emacs-and-z-shell/

function emacs-call-function() {
    local funname=$1
    echo ${(Q)~$(emacsclient -f ~/.emacs.d/server/$EMACS_SERVER_NAME \
                             -w 2                                    \
                             -e "(${funname})" |                     \
                     sed -e '1!b' -e '/emacsclient: connected to remote socket at/d')}
}

function emacs-current-filename() {
    local _location="$(emacs-call-function shell-current-buffer-filename)"
    echo "${_location}"
}
function emacs-current-filename-follow() {
    local _location="$(emacs-call-function shell-current-buffer-filename-truename)"
    echo "${_location}"
}
function emacs-current-defaultdir() {
    local _location="$(emacs-call-function shell-current-buffer-directory)"
    echo "${_location}"
}
function emacs-current-defaultdir-follow() {
    local _location="$(emacs-call-function shell-current-buffer-directory-truename)"
    echo "${_location}"
}
#
function emacs-chdir-defaultdir() {
    local _location="$(emacs-current-defaultdir)"
    if [ "$_location" ]
    then
        cd "${_location}"
    else
        echo no dir
    fi
}
function emacs-chdir-defaultdir-follow() {
    local _location="$(emacs-current-defaultdir-follow)"
    if [ "$_location" ]
    then
        cd "${_location}"
    else
        echo no dir
    fi
}

alias emacs-funcall=emacs-call-function
alias ecf=emacs-current-filename
alias ecff=emacs-current-filename-follow
alias ecd=emacs-current-defaultdir
alias ecdf=emacs-current-defaultdir-follow
#
alias cde=emacs-chdir-defaultdir
alias cdef=emacs-chdir-defaultdir-follow
##}}


