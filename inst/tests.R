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
d[1:10,]
d[,1]

## string dataset
d2 <- getH5Dataset(g, "ds_2")
d2[1:10]

## attributes
dim(a <- getH5Attribute(d2, "x"))
dim(b <- getH5Attribute(d2, "y"))
dim(c <- getH5Attribute(d2, "z"))

## > 2 dimensional data.
d3 <- getH5Dataset(g, "ds_3")

## 2 dimensional string dataset.
d4 <- getH5Dataset(g, "ds_4")
d4[,]

## instanteate a lot of objects, see if memory remains
## fixed while making new objects.
a <- replicate(100, {
  x <- replicate(100, getH5Dataset(g, "ds_4"))[1,1]
  b <- gc()[,1]
  rm(x)
  b - gc()[,1]
})

assertError <- function(expr) {
  tryCatch({{expr}; FALSE}, simpleError = function(e) {
    return(TRUE)
  })
}


##
## checks.
##
all(c(is.null(dim(d[1,10])),
      all(dim(d[,]) == dim(d)),
      is.null(dim(d2)),
      length(d2) == 20,
      is.null(dim(d2[1])),
      is.null(dim(d2[1:4])),
      is.null(dim(d3[1,1,1])),
      all(dim(d3[,,]) == dim(d3)),
      all(dim(d3[1,,]) == c(7,9)),
      all(dim(d3[,,1]) == c(3,7)),
      all(dim(d3[,1,]) == c(3,9)),
      
      ## should all be errors.
      assertError(d2[1,1:4]),
      assertError(d3[1]),
      assertError(d3[1,])))

