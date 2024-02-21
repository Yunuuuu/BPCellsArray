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

#' @include Class-BPCellsSeed.R
#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "BPCellsSeed", function(x) x)

#############################################################

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "ANY", function(x) {
    x <- coerce_dgCMatrix(x)
    methods::callGeneric()
})

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "matrix", function(x) {
    mode <- type_to_mode(storage.mode(x))
    x <- methods::as(x, "dgCMatrix")
    seed <- methods::callGeneric()
    convert_mode(seed, mode)
})
