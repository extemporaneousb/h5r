\name{H5DataFrame}
\alias{H5DataFrame}
\alias{writeH5DataFrame}
\title{
  Create an H5 Data Frame
}
\description{
  'H5DataFrame' is a function that creates a list of equal-length
  vectors. It stores H5 data tables.
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
\details{
}
\value{
}
\references{
}
\note{
}
\seealso{
}
\examples{
   require(h5r)
   h5 <- H5File("test.h5", 'w')
...
## The function is currently defined as
function (file, nms = NA) 
{
    .getCols <- function(h5) {
        nms <- names(listH5Contents(h5))
        nms[nms != "."]
    }
    h5File <- H5File(file)
    if (is.na(nms)) {
        nms <- .getCols(h5File)
    }
    h5Datasets <- lapply(nms, function(nm) {
        getH5Dataset(h5File, nm)
    })
    names(h5Datasets) <- nms
    stopifnot(length(unique(sapply(h5Datasets, length))) == 1)
    obj <- new("H5DataFrame")
    obj@h5File <- h5File
    obj@h5Datasets <- h5Datasets
    return(obj)
  }
}
\keyword{datagen}

