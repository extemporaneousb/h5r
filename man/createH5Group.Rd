\name{createH5Group}
\alias{createH5Group}
\title{
   Create a Group that Contains HDF5 Objects
}
\description{
  'createH5Group' contains zero or more HDF5 objects which can consist of
   either groups, or datasets, and supporting metadata. 
}
\usage{
  createH5Group(h5Obj, groupName, ...)
}
\arguments{
  \item{h5Obj}{
      H5 object in which the group(s) or dataset(s) are located.
}
  \item{groupName}{
      Name of group.
}
  \item{\dots}{
      Extra arguments. See class documentation for specific methods.
}
}
\details{
  HDF5 groups are comparable to directories in Linux/Unix.
}
\value{
    An object of type H5Group.   
}
\references{
  \url{http://www.hdfgroup.org/HDF5/doc/H5.intro.html.#IntroOGroups}
}
\seealso{
  \link[=HDF5 file structures]{createH5Datasets}
}
\examples{
  library("h5r")
  h5 <- H5File("test.H5",'w')
  createH5Group(h5, "grp101")
}
\keyword{datasets}

