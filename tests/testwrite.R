require(h5r)

system("rm test.h5")
h5 <- H5File("test.h5", 'w')
listH5Contents(h5)

g1 <- createH5Group( h5, "grp1" )

createH5Dataset(g1, "ds1", a <- rep(1, 100))
all(getH5Dataset(g1, "ds1")[] == a)

createH5Dataset(g1, "ds2", a <- cbind(rep(1, 10), rep(2, 10)))
all(getH5Dataset(g1, "ds2")[] == a)

createH5Dataset( g1, "ds4", rbind(rep(1, 10), rep(2, 10)))
getH5Dataset(g1, "ds4")[]

ds = createH5Dataset( g1, "ds4", data = NULL, dim = NULL, chunks = NULL)
ds[1:10, 1:10] <- dta
ds[] 



createH5Attribute( h5e, "attr1", 1:4 ) 


