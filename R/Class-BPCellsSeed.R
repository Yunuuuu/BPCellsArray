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

#' @include utils.R
#' @rdname BPCellsSeed
methods::setClass("BPCellsSeed", contains = get_class("IterableMatrix"))

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

############################################################
# Iterable_dgCMatrix_wrapper
methods::setClass("BPCellsdgCMatrixSeed",
    contains = c("BPCellsSeed", get_class("Iterable_dgCMatrix_wrapper"))
)

#' @noRd
BPCellsdgCMatrixSeed <- function(x) {
    methods::as(x, "BPCellsdgCMatrixSeed")
}

#' @export
#' @rdname BPCellsSeed
methods::setMethod(
    "BPCellsSeed", "Iterable_dgCMatrix_wrapper", function(x) {
        BPCellsdgCMatrixSeed(x = x)
    }
)

############################################################
# MatrixMultiply
methods::setClass("BPCellsMultiplySeed",
    contains = c("BPCellsSeed", get_class("MatrixMultiply")),
    slots = list(left = "BPCellsSeed", right = "BPCellsSeed")
)

#' @noRd
BPCellsMultiplySeed <- function(x) {
    x@left <- BPCellsSeed(x@left)
    x@right <- BPCellsSeed(x@right)
    methods::as(x, "BPCellsMultiplySeed")
}

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "MatrixMultiply", function(x) {
    BPCellsMultiplySeed(x = x)
})

############################################################
# RenameDims
methods::setClass("BPCellsRenameDimsSeed",
    contains = c("BPCellsSeed", get_class("RenameDims")),
    slots = list(matrix = "BPCellsSeed")
)

#' @noRd
BPCellsRenameDimsSeed <- function(x) {
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsRenameDimsSeed")
}

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "RenameDims", function(x) {
    BPCellsRenameDimsSeed(x = x)
})

############################################################
# MatrixSubset
methods::setClass("BPCellsSubsetSeed",
    contains = c("BPCellsSeed", get_class("MatrixSubset")),
    slots = list(matrix = "BPCellsSeed")
)

#' @noRd
BPCellsSubsetSeed <- function(x) {
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsSubsetSeed")
}

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "MatrixSubset", function(x) {
    BPCellsSubsetSeed(x = x)
})
