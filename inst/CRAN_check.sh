#!/bin/bash

RCMD=`which R`
TARGET=h5r

# build installable package.
$RCMD CMD build $TARGET

BUILT_TARGET=h5r_`cat $TARGET/DESCRIPTION | grep Version | sed 's/Version: //g'`.tar.gz

# check according to CRAN
$RCMD CMD CHECK --as-cran --timings --install-args="--no-lock --preclean" $BUILT_TARGET


