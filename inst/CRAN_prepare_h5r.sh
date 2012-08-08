#!/bin/sh

SOURCE=h5r
TARGET=scratch/h5r-CRAN

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
R CMD build $TARGET

# 5. check
R CMD check --as-cran --timings --install-args="--no-lock --preclean" `ls h5r_*tar.gz`
