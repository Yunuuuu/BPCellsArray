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

##############################################################
BPCellsSubset_internal <- function(x, i, j, ..., drop = FALSE) {
    BPCellsSeed(methods::callNextMethod())
}

#' @param i,j Row and Column index.
#' @param drop Ignored, always be `FALSE`.
#' @importMethodsFrom BPCells [
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsdgCMatrixSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsDirSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsHDF5Seed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
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
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsConvertSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsMaskSeed", BPCellsSubset_internal)

# it's not necessary to re-dispatch the "[" method for `BPCellsMultiplySeed`
# class since the `MatrixMultiply` method will use `[` method of `@left` and
# `@right`. Here, we just re-dispatch it to keep consistent.
#' @importMethodsFrom BPCells [
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsMultiplySeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsRankTransformSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsRenameDimsSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsSubsetSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsTransformedSeed", BPCellsSubset_internal)
