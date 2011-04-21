require(h5r)


system("rm test.h5")
h5 <- H5File("test.h5", 'w')

listH5Contents(h5)

g1 <- createH5Group( h5, "grp1" )
d <- createH5Dataset(g1, "ds3", dType = "integer", dims = 100)
writeH5Data(d, as.integer(1:10), 0, 10)
writeH5Data(d, as.integer(1:10), 11, 4)

d <- createH5Dataset(g1, "ds4", dType = "integer", dims = c(10, 2))
writeH5Data(d, as.integer(1:10), c(0, 0), c(5, 2))
writeH5Data(d, as.integer(1:10), c(0, 0), c(10, 2))

d <- createH5Dataset(g1, "ds5", rbind(rnorm(10), rnorm(10)))
d[]
