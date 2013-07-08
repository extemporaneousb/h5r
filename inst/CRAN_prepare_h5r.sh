#!/bin/sh

SOURCE=h5r
TARGET=scratch/h5r-CRAN
RCMD=/home/UNIXHOME/jbullard/projects/software/R/common/R-3.1-install/bin/R

rm -rf $TARGET
rsync -av $SOURCE/ $TARGET/

# instructions from Uwe. 
# 1. remove the windows directory
rm -rf $TARGET/windows

# 2. remove configure.win and cleanup.win
rm -f $TARGET/configure.win
rm -f $TARGET/cleanup.win
rm -f $TARGET/src/Makevars.win
rm -f $TARGET/inst/CRAN_*

# 3. make Makevars.win
echo 'PKG_CFLAGS = -I$(LIB_HDF5)$(R_ARCH)/include -DWINDOWS' > $TARGET/src/Makevars.win
echo 'PKG_LIBS = -lm -L$(LIB_HDF5)$(R_ARCH)/lib -lhdf5 -lsz -lz' >> $TARGET/src/Makevars.win

# 4. build
$RCMD CMD build $TARGET

BUILT_TARGET=h5r_`cat $TARGET/DESCRIPTION | grep Version | sed 's/Version: //g'`.tar.gz

# 5. check
$RCMD CMD check --as-cran --timings --install-args="--no-lock --preclean" $BUILT_TARGET

