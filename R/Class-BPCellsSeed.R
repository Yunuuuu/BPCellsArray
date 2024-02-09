#' Low-level Base Class for Delayed BPCells matrix
#'
#' The `BPCellsSeed` class just inherits from the `IterableMatrix` class in
#' BPCells package. The purpose for `BPCellsSeed` class is to provide the common
#' methods for all low-level BPCells seed classes.
#'
#' @param x A `IterableMatrix` object from `BPCells`, a matrix-like object which
#' can be coerced into dgCMatrix, or a `BPCellsSeed` object.
#' @name BPCellsSeed
NULL

# BPCells matrix object indeed is a DelayedMatrix, but we regard it as a seed
# object in DelayedMatrix
# we cannot use the `DelayedNaryOp` object since it use `seeds` slot to save a
# list of other seed objects while BPCells use left (and right) or matrix_list.

#' @include utils.R
#' @rdname BPCellsSeed
methods::setClass("BPCellsSeed",
    contains = c(BPCells_class("IterableMatrix"), "VIRTUAL")
)

methods::setValidity("BPCellsSeed", function(object) {
    if (length(dim(object)) != 2L) {
        cli::cli_abort("{.pkg BPCells} can only support 2-dim matrix")
    }
    TRUE
})

methods::setClass("BPCellsBasicSeed", contains = c("BPCellsSeed", "VIRTUAL"))
methods::setClass("BPCellsUnaryOpsSeed", contains = c("BPCellsSeed", "VIRTUAL"))
methods::setClass("BPCellsNaryOpsSeed", contains = c("BPCellsSeed", "VIRTUAL"))

#############################################################
# used to extract the actual entity of `BPCellsSeed` objet.
methods::setGeneric("entity", function(x, ...) standardGeneric("entity"))
methods::setMethod("entity", "BPCellsBasicSeed", function(x) x)
methods::setMethod("entity", "BPCellsUnaryOpsSeed", function(x) x@matrix)

# nary_seeds <- c(
#     "BPCellsBindMatrixSeed", "BPCellsMaskSeed",
#     "BPCellsMultiplySeed"
# )
# unary_seeds <- c(
#     "BPCellsConvertSeed", "BPCellsRankTransformSeed",
#     "BPCellsRenameDimsSeed", "BPCellsSubsetSeed", "BPCellsTransformedSeed"
# )

############################################################
#' @export
#' @rdname BPCellsSeed
methods::setGeneric("BPCellsSeed", function(x) {
    standardGeneric("BPCellsSeed")
})

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
