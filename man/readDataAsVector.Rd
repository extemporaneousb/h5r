\name{readDataAsVector}
\alias{readDataAsVector}
\alias{readDataAsVector,H5Dataset-method}
\alias{readDataAsVector,H5Attribute-method}
\title{
  Reads the data from a dataset as a vector with no dimensions. Useful
  internally as well as the case when one wants to shape differently. 
}
\description{
  This function returns a vector of the same type as the dataset with
  length: \code{"prod(dim(h5Dataset))"}.
}
\usage{
readDataAsVector(h5Obj, ...)
}
\arguments{
  \item{h5Obj}{Either a H5Dataset or H5Attribute.}
  \item{...}{Standard generic definition.}
}
\examples{
 f <- system.file("h5_files", "ex_1.h5", package = "h5r")
 h5 <- H5File(f)
 d <- getH5Dataset(getH5Group(h5, "group_1"), "ds_1")
 head(readDataAsVector(d))
}
\keyword{file}
