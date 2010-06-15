from h5py import *
from numpy import *
import time

h5 = File("/home/NANOFLUIDICS/jbullard/local/big.h5")
ds = h5["cdata_1e3"]
N  = 100

def f(ds, N):
    start = random.randint(0, len(ds), N)
    end   = start + random.exponential(1000, N) + 1
    end[end > len(ds)] = len(ds)
    
    for j in zip(start, end):
        z = ds[j[0]:j[1]]
    return True


def myTime(K, ds, N):
    res = [0]*K

    for i in range(0, K):
        s = time.time()
        f(ds, N)
        res[i] = time.time() - s
    return res

times = myTime(100, ds, N = 1000)
o     = file('pyres.txt', 'w')
o.write(" ".join(map(str, times)))
o.close()
