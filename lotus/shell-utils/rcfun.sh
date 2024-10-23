# -*- Mode: shell-script; indent-tabs-mode: nil; sh-shell: zsh; -*-


##{{ emacs
# from: https://jpace.wordpress.com/2016/12/01/current-file-and-directory-in-emacs-and-z-shell/

function emacs-source-shell-rcfun() {

    if typeset -f emacs-client-call-function > /dev/null 2>&1
    then
        local filename="$(emacs-client-call-function shell-rcfun-location)"
        if [ "${filename}" ]
        then
            source ${filename}
        fi
    else
        echo Define emacs-client-call-function function to invoke elisp function. >&2
    fi
}
alias emacs-shell-setup=emacs-source-shell-rcfun

function emacs-call-function() {
    local funname=$1
    emacs-client-call-function "$1"
}

function emacs-current-buffer-filename() {
    local _location="$(emacs-call-function shell-current-buffer-filename)"
    echo "${_location}"
}
function emacs-current-buffer-filename-truename() {
    local _location="$(emacs-call-function shell-current-buffer-filename-truename)"
    echo "${_location}"
}
function emacs-current-buffer-default-directory() {
    local _location="$(emacs-call-function shell-current-buffer-default-directory)"
    echo "${_location}"
}
function emacs-current-buffer-default-directory-truename() {
    local _location="$(emacs-call-function shell-current-buffer-default-directory-truename)"
    echo "${_location}"
}
#
function emacs-chdir-defaultdir() {
    local _location="$(emacs-current-buffer-default-directory)"
    if [ "$_location" ]
    then
        cd "${_location}"
    else
        echo no dir
    fi
}
function emacs-chdir-defaultdir-truename() {
    local _location="$(emacs-current-buffer-default-directory-truename)"
    if [ "$_location" ]
    then
        cd "${_location}"
    else
        echo no dir
    fi
}

alias emacs-funcall=emacs-call-function
alias ecf=emacs-current-buffer-filename
alias ecff=emacs-current-buffer-filename-truename
alias ecd=emacs-current-buffer-default-directory
alias ecdf=emacs-current-buffer-default-directory-truename
#
alias cde=emacs-chdir-defaultdir
alias cdef=emacs-chdir-defaultdir-truename
##}}


