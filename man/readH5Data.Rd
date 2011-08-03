\name{readH5Data}
\alias{readH5Data}
\title{
  Print H5 data in the specified H5 Object
}
\description{
  'readH5Data' is a function that is used for printing out a dataset in
  the specified H5 Object.
}
\usage{
readH5Data(h5Obj, ...)
}
\arguments{
  \item{h5Obj}{
    H5 object in which the dataset is located.
}
  \item{\dots}{
    Extra arguments. See class documentation for specific methods.
}
}
\value{
  The values returned depend on H5 dataset data. Values returned can be
  of type integer, double or character.
}
\examples{
  require(h5r)
  h5 <- H5File("test.H5",'w')
  m <- createH5Dataset(h5, "mm", cbind(rnorm(1000), rnorm(1000)))
  readH5Data(m)
}
\keyword{datasets}

