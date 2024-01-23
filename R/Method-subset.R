#' @importMethodsFrom BPCells [
#' @export
#' @rdname seed-methods
methods::setMethod(
    "[", "BPCellsColBindMatrixSeed", function(x, i, j, ..., drop = FALSE) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname seed-methods
methods::setMethod(
    "[", "BPCellsRowBindMatrixSeed", function(x, i, j, ..., drop = FALSE) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @importMethodsFrom BPCells [
#' @rdname seed-methods
methods::setMethod(
    "[", "BPCellsConvertSeed", function(x, i, j, ..., drop = FALSE) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @importMethodsFrom BPCells [
#' @rdname seed-methods
methods::setMethod(
    "[", "BPCellsdgCMatrixSeed", function(x, i, j, ..., drop = FALSE) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @importMethodsFrom BPCells [
#' @rdname seed-methods
methods::setMethod(
    "[", "BPCellsDirSeed", function(x, i, j, ..., drop = FALSE) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @importMethodsFrom BPCells [
#' @rdname seed-methods
methods::setMethod(
    "[", "BPCellsMaskSeed", function(x, i, j, ..., drop = FALSE) {
        BPCellsSeed(methods::callNextMethod())
    }
)

# it's not necessary to re-dispatch the "[" method for `BPCellsMultiplySeed`
# class since the `MatrixMultiply` method will use `[` method of `@left` and
# `@right`. Here, we just re-dispatch it to keep consistent.
#' @importMethodsFrom BPCells [
#' @rdname seed-methods
methods::setMethod(
    "[", "BPCellsMultiplySeed", function(x, i, j, ..., drop = FALSE) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @importMethodsFrom BPCells [
#' @rdname seed-methods
methods::setMethod(
    "[", "BPCellsRankTransformSeed", function(x, i, j, ..., drop = FALSE) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @importMethodsFrom BPCells [
#' @rdname seed-methods
methods::setMethod(
    "[", "BPCellsRenameDimsSeed", function(x, i, j, ..., drop = FALSE) {
        BPCellsSeed(methods::callNextMethod())
    }
)

# it's not necessary to re-dispatch the "[" method for `BPCellsSubsetSeed` class
# since the `MatrixSubset` method will use `[` method of `@matrix`. Here, we
# just re-dispatch it to keep consistent.
#' @importMethodsFrom BPCells [
#' @rdname seed-methods
methods::setMethod(
    "[", "BPCellsSubsetSeed", function(x, i, j, ..., drop = FALSE) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @importMethodsFrom BPCells [
#' @rdname seed-methods
methods::setMethod(
    "[", "BPCellsTransformedSeed", function(x, i, j, ..., drop = FALSE) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @importMethodsFrom BPCells [
#' @rdname seed-methods
methods::setMethod(
    "[", "BPCellsMemSeed", function(x, i, j, ..., drop = FALSE) {
        BPCellsSeed(methods::callNextMethod())
    }
)
