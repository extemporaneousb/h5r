##
## Used to test the examples in the h5_files directory.
##
require(h5r)

files <- list.files("h5_files", full.names = TRUE)

## ex_1
f <- H5File(files[1])
g <- getH5Group(f, "group_1")
d <- getH5Dataset(g, "ds_1")
d[1:10, 1:10]

## string dataset
d2 <- getH5Dataset(g, "ds_2")
d2[1:10]

## attributes
dim(a <- getH5Attribute(d2, "x"))
dim(b <- getH5Attribute(d2, "y"))
dim(c <- getH5Attribute(d2, "z"))

## > 2 dimensional data.
d3 <- getH5Dataset(g, "ds_3")

d4 <- getH5Dataset(g, "ds_4")
d4[,]
