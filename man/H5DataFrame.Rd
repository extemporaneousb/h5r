\name{H5DataFrame}
\alias{H5DataFrame}
\alias{writeH5DataFrame}
\title{
  Create an H5 Data Frame
}
\description{
  'H5DataFrame' is a function that creates a list of equal-length
  vectors. 
}
\usage{
H5DataFrame(file, nms = NA)
}
\arguments{
  \item{file}{
    H5 file.
  }
  \item{nms}{
    File names.
  }
}
\examples{
   require(h5r)
   h5 <- H5File("test.h5", 'w')
}
\keyword{datagen}

