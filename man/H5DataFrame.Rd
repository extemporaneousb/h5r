\name{H5DataFrame}
\alias{H5DataFrame}
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
    
}
  \item{nms}{
%%     ~~Describe \code{nms} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
   require(h5r)
   h5 <- H5File("test.h5", 'w')
   d1 <- createH5Dataset(h5,"one", c(1,7,5))
   d2 <- createH5Dataset(h5,"two", c(6,4,8))
   ...

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
