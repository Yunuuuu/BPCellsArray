.onLoad <- function(libname, pkgname) {
    old_seedApply <- DelayedArray::seedApply
    rebind("seedApply", function(x, FUN, ...) {
        assert_(FUN, is.function, "a function")
        if (methods::is(x, c("BPCellsMatrix", "BPCellsSeed"))) {
            new_seedApply(x, FUN, ...)
        } else {
            old_seedApply(x, FUN, ...)
        }
    }, "DelayedArray")
}
