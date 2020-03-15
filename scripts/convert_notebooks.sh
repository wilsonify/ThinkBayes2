#!/bin/bash
SCRIPTPATH="$(
  cd "$(dirname "$0")" >/dev/null 2>&1
  pwd -P
)"

PARENTPATH=$SCRIPTPATH/..
NOTEBOOKSPATH=$PARENTPATH/notebooks
SRCPATH=$PARENTPATH/src

find $NOTEBOOKSPATH -type f -name '*.ipynb' | while read infile; do
  outfile="${infile%.ipynb}.py"
  jupytext --to py "$infile" --output "$outfile"
done
