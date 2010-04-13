\name{readDataAsVector}
\alias{readDataAsVector}
\title{
  Reads the data from a dataset as a vector with no dimensions. Useful
  internally as well as the case when one wants to shape differently. 
}
\description{
  This function returns a vector of the same type as the dataset with
  length: \code{"prod(dim(h5Dataset))"}.
}
\usage{
readDataAsVector(h5Dataset)
}
\arguments{
  \item{h5Dataset}{An object of class H5Dataset.
  }
}
\examples{
 f <- system.file("h5_files", "ex_1.h5", package = "h5R")
 h5 <- H5File(f)
 d <- getH5Dataset(getH5Group(h5, "group_1"), "ds_1")
 head(readDataAsVector(d))
}
\keyword{file}
