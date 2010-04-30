##
## Used to test hyperslab reading.
##
require(h5r)

files <- list.files("h5_files", full.names = TRUE)

## ex_1
f <- H5File(files[1])
g <- getH5Group(f, "group_1")
d <- getH5Dataset(g, "ds_6", inMemory = FALSE)
d2 <- getH5Dataset(g, "ds_6", inMemory = TRUE)

all(d[,] == d2[,])
all(d[] == d2[])
all(d[2:length(d)] == d2[2:length(d2)])

timeMe <- function(d) {
  k <- 1000
  n <- 1000
  system.time({
    for (i in seq.int(1, n)) {
      b <- runif(1, 1, nrow(d) - k)
      length(readSlab(d, b, k))
    }
  })
}
timeMe(d)
timeMe(d2)

dM <- getH5Dataset(g, "ds_1", inMemory = FALSE)
dMM <- getH5Dataset(g, "ds_1", inMemory = TRUE)

all(dM[] == dMM[])
all(dM[,] == dMM[,])
all(dM[1:5, 1:5] == dMM[1:5, 1:5])
all(dM[1:5, 2] == dMM[1:5, 2])

length(dM[1:5, 2]) == length(dMM[1:5, 2])

all(dim(dM[1:5, 2:3]) == dim(dMM[1:5, 2:3]))


##
## Three dimensional 
##
d3M <- getH5Dataset(g, "ds_3", inMemory = TRUE)
d3 <- getH5Dataset(g, "ds_3", inMemory = FALSE)

d3M[,,]
d3[,,]

d3[1,,]
d3[1,1,1]
d3[1,,]
d3[,,1]


dStr <- getH5Dataset(g, "ds_2", inMemory = FALSE)
dStrM <- getH5Dataset(g, "ds_2", inMemory = TRUE)
all(dStr[1:2] == dStrM[1:2])
all(dStr[1:2] == dStrM[1:2])


dStr <- getH5Dataset(g, "ds_4", inMemory = FALSE)
dStrM <- getH5Dataset(g, "ds_4", inMemory = TRUE)
all(dStr[1:2,] == dStrM[1:2,])
all(dStr[,1:2] == dStrM[,1:2])

x = replicate(1000000, dStr[,1:5])

