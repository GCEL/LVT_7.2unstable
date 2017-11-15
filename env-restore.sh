# restore saved envionment varibles saved with `export -p > saved_vars`
# restore

blacklisted () {
  case $1 in
    PWD|OLDPWD|SHELL|STORAGE|-*) return 0 ;;
    *) return 1 ;;
  esac
}

eval '
  export() {
    blacklisted "${1%%=*}" || unset -v "${1%%=*}"
  }
  '"$(export -p)"
export() {
  blacklisted "${1%%=*}" || command export "$@"
}
. saved-env
unset -f export
