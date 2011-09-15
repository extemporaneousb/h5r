##
## Currently, it seems as if I hold on to too much memory - this
## corresponds to me not cleaning something up in HDF5 because
## Valgrind says I'm fine.
##
require(h5r)

m <- .Call("h5R_allocate_gig")
rm(m)
gc()

m <- sapply(1:1000, function(a) {
  .Call("h5R_allocate_meg")
})
rm(m)
gc()

m <- sapply(1:100000, function(a) {
  .Call("h5R_allocate_k")
})
rm(m)
gc()
