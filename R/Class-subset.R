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

################    BPCellsMatrix Methods    ##################
#' @inheritParams BPCellsSeed-methods
#' @return
#' - `[`: A [BPCellsMatrix] object.
#' @order 2
#' @export
#' @rdname BPCellsMatrix-methods
methods::setMethod(
    "[", c("BPCellsMatrix", "ANY", "ANY"),
    function(x, i, j, ..., drop = FALSE) {
        DelayedArray(x@seed[i, j, ..., drop = drop])
    }
)

#' @inheritParams BPCellsSeed-methods
#' @export
#' @rdname internal-methods
methods::setMethod(
    "[", c("BPCellsMatrix", "missing", "ANY"),
    function(x, i, j, ..., drop = FALSE) {
        DelayedArray(x@seed[, j, ..., drop = drop])
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "[", c("BPCellsMatrix", "ANY", "missing"),
    function(x, i, j, ..., drop = FALSE) {
        DelayedArray(x@seed[i, , ..., drop = drop])
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "[", c("BPCellsMatrix", "missing", "missing"),
    function(x, i, j, ..., drop = FALSE) {
        return(x)
    }
)

################    BPCellsSeed Methods    ########################
# Don't use BPCellsSeed to implement `[` method since it will dispatch
# IterableMatrix method but some classes of BPCells do have their own `[`
# method, so we re-dispatch method for every seed class.
BPCellsSubset_internal <- function(x, i, j, ..., drop = FALSE) {
    BPCellsSeed(methods::callNextMethod())
}

#' @param i,j Row and Column index.
#' @param drop Ignored, always be `FALSE`.
#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsdgCMatrixSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsDirSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsHDF5Seed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsMemSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsColBindMatrixSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsRowBindMatrixSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsConvertSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsMaskSeed", BPCellsSubset_internal)

# it's not necessary to re-dispatch the "[" method for `BPCellsMultiplySeed`
# class since the `MatrixMultiply` method will use `[` method of `@left` and
# `@right`. Here, we just re-dispatch it to keep consistent.
#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsMultiplySeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsRankTransformSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsRenameDimsSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsSubsetSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsTransformedSeed", BPCellsSubset_internal)
