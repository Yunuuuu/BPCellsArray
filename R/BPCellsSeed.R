#' Build `BPCellsSeed` object
#'
#' @param x A `IterableMatrix` object from `BPCells`, a matrix-like object which
#' can be coerced into dgCMatrix, or a [BPCellsSeed][BPCellsSeed-class] object.
#' @return A [BPCellsSeed][BPCellsSeed-class] object.
#' @name BPCellsSeed
#' @seealso [BPCellsSeed-class][BPCellsSeed-class]
NULL

############################################################
#' @export
#' @rdname BPCellsSeed
methods::setGeneric("BPCellsSeed", function(x) {
    standardGeneric("BPCellsSeed")
})

############################################################
#' @include Class-BPCellsSeed.R
#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "IterableMatrix", function(x) x)

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "BPCellsDelayedOp", function(x) x)

##############################################################
#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "matrix", function(x) {
    mode <- storage_mode(x)
    x <- methods::as(x, "dgCMatrix")
    seed <- methods::callGeneric()
    convert_mode(seed, mode)
})

############################################################
# Iterable_dgCMatrix_wrapper
summary.Iterable_dgCMatrix_wrapper <- function(object) {
    "Load dgCMatrix from memory"
}

methods::setMethod(
    "summary", "Iterable_dgCMatrix_wrapper",
    summary.Iterable_dgCMatrix_wrapper
)

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "dgCMatrix", function(x) {
    methods::as(x, "IterableMatrix")
})

#############################################################
#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "ANY", function(x) {
    x <- coerce_dgCMatrix(x)
    methods::callGeneric()
})
