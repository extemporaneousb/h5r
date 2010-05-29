from h5py import *
from numpy import *
import glob

h5 = File("/home/jbullard/big.h5")
x  = random.randint(0, 1e8, 1e8)

## with chunks.
h5.create_dataset("cdata_1e3", data = x, chunks = (1000,))
h5.create_dataset("cdata_1e4", data = x, chunks = (10000,))
h5.create_dataset("cdata_1e5", data = x, chunks = (100000,))
h5.create_dataset("cdata_1e6", data = x, chunks = (1000000,))

## without
h5.create_dataset("udata", data = x)

## zipped
h5.create_dataset("zdata", data = x, compression = 'gzip')

## performance.


h5.close()


