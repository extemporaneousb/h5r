from h5py import *
from numpy import *
import glob

h5 = File(glob.glob("~/h5_files/big.h5"))
x  = random.randint(0, 1e5, 1e9)

## with chunks.
h5.create_dataset("cdata", data = x, chunks = (10000,))

## without
h5.create_dataset("udata", data = x)

## zipped
h5.create_dataset("zdata", data = x, compression = 'gzip')

## performance.


h5.close()


