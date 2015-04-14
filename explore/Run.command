#!/bin/sh
DIRNAME=`dirname "$0"`
cd $DIRNAME

exists () {
  type "$1" >/dev/null 2>/dev/null
}

if [ $# -gt 0 ]; then
    export PATH=$PATH:"$1"
    echo "Added $1 to path temporarily"
fi

if exists Rscript; then
    echo "Found Rscript: " `type Rscript`
    Rscript config.R
else
    echo "Rscript not found in PATH."
    echo "Enter path to Rscript (ie. /usr/bin/Rscript) [Control-C Control-C to exit]:"
    read path
    ./$0 path
fi
