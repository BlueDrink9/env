#!/bin/sh

extractPSCmdCol(){
  # The PS COMMAND col is sometimes abbreviated CMD
  awk -v p='C(OM)?M(AN)?D' 'NR==1 {n=match($0, p); next} {print substr($0, n)}'
}
getShellProgram(){
  # Get cmd of current process, last row of ps, strip leading - (osx) and trailing args.
  ps -p $$ | extractPSCmdCol | tail -1 | sed 's/^-//' | sed 's/ -.*//'
}
