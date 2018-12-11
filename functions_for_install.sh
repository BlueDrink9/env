#!/usr/bin/env bash
# vim: foldmethod=marker
# vim: foldmarker={[},{]}

# For debugging use
# set -eEuxo pipefail
# set -uxo pipefail

# {[} Utility functions
printLine() {
    printf -- "%s\n" "$@"
}
printErr() {
    >&2 printLine "$@"
}

askQuestionYN() {
    default="?"
    question=${1:-default}
    echo -ne "${question} (y/n) " >&2
    read -n 1 REPLY
    printErr ""
    if [[ $REPLY =~ ^[yY]$ ]]; then
        return 0
    else
        return 1
    fi
}

# {]} Utility functions
