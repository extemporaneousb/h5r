require(h5r)

require(h5r)
h5 <- H5File("test.h5", 'w')
d <- createH5Dataset(h5, "jim", c('jime ', 'joe', 'mikey'))

d <- createH5Dataset(h5, "dd", rnorm(1000000))

s <- sample(1:length(d), size = 10000)
system.time(z <- d[s])
system.time(y <- readPoints(d, s-1))
all(z==y)

m <- createH5Dataset(h5, "mm", cbind(rnorm(1000), rnorm(1000)))

d <- createH5Dataset(h5, "dusdf", as.character(1:20))
writeH5Data(d, "jim", 1, 1)
