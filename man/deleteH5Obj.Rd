\name{deleteH5Obj}
\alias{deleteH5Obj}
\title{
  Delete Object
}
\description{
  'deleteH5Obj' is a function that removes the H5 Object. The H5 object,
  which is a baseclass, contains three subclasses: group, dataset and
  datatype.
}
\usage{
deleteH5Obj(h5Obj, h5ObjName, ...)
}
\arguments{
  \item{h5Obj}{
    This is the H5Object itself.
}
  \item{h5ObjName}{
    Name of H5Object.
}
  \item{\dots}{
    Extra arguments. See class documentation for specific methods.
}
}
\value{
  An object of type H5Object.
}
\references{
  \link{http://www.hdfgroup.org/HDF5/doc/cpplus_RM/classH5_1_1H5Object.html}
}
\seealso{
  \link[=HDF5 groups]{createH5Group}
}  
\examples{
   require(h5r)
   h5 <- H5File("test.h5", 'w')
   g1 <- createH5Group(h5,"grp1")
   dl <- deleteH5Obj(h5,"grp1")
   dl
}
\keyword{data}

