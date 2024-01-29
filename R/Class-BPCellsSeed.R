#' Low-level Base Class for Delayed BPCells matrix
#'
#' The `BPCellsSeed` class just inherits from the `IterableMatrix` class in
#' BPCells package. The purpose for `BPCellsSeed` class is to provide the common
#' methods for all low-level BPCells seed classes.
#'
#' @param x A `BPCellsSeed` object or a `IterableMatrix` object from BPCells,
#' details see the method signature.
#' @name BPCellsSeed
NULL

# BPCells matrix object indeed is a DelayedMatrix, but we regard it as a seed
# object in DelayedMatrix
# we cannot use the `DelayedNaryOp` object since it use `seeds` slot to save a
# list of other seed objects while BPCells use left (and right) or matrix_list.


#' @include utils.R
#' @rdname BPCellsSeed
methods::setClass("BPCellsSeed",
    contains = c(get_class("IterableMatrix"), "VIRTUAL")
)
methods::setClass("BPCellsUnaryOpsSeed", contains = c("BPCellsSeed", "VIRTUAL"))
methods::setClass("BPCellsNaryOpsSeed", contains = c("BPCellsSeed", "VIRTUAL"))

#' @export
#' @rdname BPCellsSeed
methods::setGeneric("BPCellsSeed", function(x) {
    standardGeneric("BPCellsSeed")
})

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "BPCellsSeed", function(x) {
    x
})

#############################################################
# used to extract the actual entity of `BPCellsSeed` objet.
methods::setGeneric("entity", function(x, ...) standardGeneric("entity"))
methods::setMethod("entity", "BPCellsSeed", function(x) x)
methods::setMethod("entity", "BPCellsUnaryOpsSeed", function(x) {
    x@matrix
})

# nary_seeds <- c(
#     "BPCellsBindMatrixSeed", "BPCellsMaskSeed",
#     "BPCellsMultiplySeed"
# )
# unary_seeds <- c(
#     "BPCellsConvertSeed", "BPCellsRankTransformSeed",
#     "BPCellsRenameDimsSeed", "BPCellsSubsetSeed", "BPCellsTransformedSeed"
# )

############################################################
# Iterable_dgCMatrix_wrapper
methods::setClass("BPCellsdgCMatrixSeed",
    contains = c("BPCellsSeed", get_class("Iterable_dgCMatrix_wrapper"))
)

#' @noRd
BPCellsdgCMatrixSeed <- function(x) {
    methods::new(
        "BPCellsdgCMatrixSeed",
        dim = dim(x), dimnames = dimnames(x),
        transpose = x@transpose, mat = x@mat
    )
}

#' @export
#' @rdname BPCellsSeed
methods::setMethod(
    "BPCellsSeed", "Iterable_dgCMatrix_wrapper", function(x) {
        BPCellsdgCMatrixSeed(x = x)
    }
)

#' @export
methods::setAs("dgCMatrix", "BPCellsdgCMatrixSeed", function(from) {
    methods::new(
        "BPCellsdgCMatrixSeed",
        dim = dim(from), dimnames = dimnames(from),
        transpose = FALSE, mat = from
    )
})

#' @export
methods::setAs("ANY", "BPCellsdgCMatrixSeed", function(from) {
    methods::as(coerce_dgCMatrix(from), "BPCellsdgCMatrixSeed")
})

############################################################
