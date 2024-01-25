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

# we cannot use the `DelayedNaryOp` object since it use `seeds` slot to save a
# list of other seed objects.
#
# here: we allow seed to return a list of seeds object, which is different with
# what DelayedArray dose, this is because that seedApply() function is not a
# generic function, we cannot define methods for it. To allow, seed return a
# list of object, we can also use `seedApply` as usual.
#
# BPCells matrix object indeed is a DelayedMatrix, but we regard it as a seed
# object in DelayedMatrix

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
    contains = c("BPCellsNaryOpsSeed", get_class("MatrixMultiply")),
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

methods::setMethod("entity", "BPCellsMultiplySeed", function(x) {
    list(left = x@left, right = x@right)
})

############################################################
# RenameDims
methods::setClass("BPCellsRenameDimsSeed",
    contains = c(
        "BPCellsUnaryOpsSeed",
        get_class("RenameDims")
    ),
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
    contains = c(
        "BPCellsUnaryOpsSeed",
        get_class("MatrixSubset")
    ),
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
