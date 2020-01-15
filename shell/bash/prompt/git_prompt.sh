#!/usr/bin/env bash
# vim: set ft=sh:
# vim:ts=2:sw=2
#
# blueDrink9 custom bash prompt. Relies on 2 other files.
# Assumes a solarised terminal, with 16 colours.

# For substrInStr
source ${SCRIPT_DIR}/../../functions.sh

strInText(){
  text="$1"
  str="$2"
  # After profiling this, I've discovered that my method that uses builtins
  # is waaaaayyy faster than grep. ~ 100x faster.
  # Probably moot, since it only runs about 10 times, but still something I
  # would rather avoid, esp on slower machines.
  # Note the extra 2 0's in the first loop range.
  #
  # gstatus="$(git status)"
  # time (for i in {1..90000}; do substrInStr "modified:" "${gstatus}"; done)
  # real    0m6.743s
  # time (for i in {1..900}; do echo -n "${gstatus}" 2> /dev/null | grep "modified:" > /dev/null; done)
  # real    0m3.126s

  # echo -n "${text}" 2> /dev/null | grep "${str}" &> /dev/null

  # Blank strings are false when tested with [ '' ]. Only return if true.
  if substrInStr "${str}" "${text}"; then
    echo 0;
  fi
}

# get current branch in git repo
# Check status of branch
# pgreen if no changes, yellow if modified. Cyan if there are misc changes to files.
prompt_parse_git_branch() {
  STATUS_COLOUR=${pNC}
  BRANCH=$(get_git_branch)
  if [ ! "${BRANCH}" = "" ]; then
    status=$(git status 2>&1 | tee)
    dirty=$(strInText "${status}" "modified:")
    clean=$(strInText "${status}" "clean")
    untracked=$(strInText "${status}" "Untracked files")
    ahead=$(strInText "${status}" "Your branch is ahead of")
    behind=$(strInText "${status}" "Your branch is behind")
    diverged=$(strInText "${status}" "diverged")
    newfile=$(strInText "${status}" "new file:")
    renamed=$(strInText "${status}" "renamed:")
    deleted=$(strInText "${status}" "deleted:")
    conflicted=$(strInText "${status}" "Unmerged:")
    bits=''
    if [ "${clean}" ]; then bits=""; STATUS_COLOUR=${pgreen}; fi
    if [ "${ahead}" ]; then bits="^${bits}"; STATUS_COLOUR=${pcyan}; fi
    if [ "${behind}" ]; then bits="v${bits}"; STATUS_COLOUR=${pcyan}; fi
    # optional: use several other possible unicode symbols.
    if [ "${diverged}" ]; then bits="^v${bits}"; STATUS_COLOUR=${pcyan}; fi
    if [ "${untracked}" ]; then bits="?${bits}"; fi
    if [ "${renamed}" ]; then bits=">${bits}"; fi
    if [ "${deleted}" ]; then bits="X${bits}"; fi
    if [ "${newfile}" ]; then bits="+${bits}"; fi
    if [ "${dirty}" ]; then bits="~${bits}"; STATUS_COLOUR="${pyellow}"; fi
    if [ "${conflicted}" ]; then bits="!${bits}"; STATUS_COLOUR="${pred}"; fi
    if [ ! "${bits}" = "" ] || [ "${clean}" = "0" ]; then
      STATUS="${bits}"
    else
      STATUS="!"
    fi

    # Sometimes status can be slow. Consider removing.
    GIT_PROMPT="${STATUS_COLOUR}{${BRANCH}${STATUS}}${pNC}"
  else
    GIT_PROMPT=""
  fi
  echo "${GIT_PROMPT}"
  unset GIT_PROMPT STATUS_COLOUR BRANCH bits deleted renamed newfile \
    diverged behind ahead untracked clean dirty status STATUS
}

