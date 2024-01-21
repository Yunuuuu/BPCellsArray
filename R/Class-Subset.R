#' Delayed BPCells MatrixSubset
#'
#' The `BPCellsSubsetArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `MatrixSubset` object
#' in BPCells.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' `[` (extract methods) of other BPCellsArray objects.
#'
#' @export
#' @name BPCellsSubset
methods::setClass("BPCellsSubsetSeed",
    contains = c("BPCellsSeed", get_class("MatrixSubset")),
    slots = list(matrix = "BPCellsSeed")
)

#' @param x A `MatrixSubset` or `BPCellsSubsetSeed` object.
#' @export
#' @rdname BPCellsSubset
BPCellsSubsetSeed <- function(x) {
    assert_s4_class(x, "MatrixSubset")
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsSubsetSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsSubset
methods::setClass("BPCellsSubsetArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsSubsetSeed")
)

#' @param seed A `BPCellsSubsetSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @export
#' @rdname BPCellsSubset
methods::setMethod(
    "DelayedArray", "BPCellsSubsetSeed",
    function(seed) new_DelayedArray(seed, Class = "BPCellsSubsetArray")
)

#' @export
#' @rdname BPCellsSubset
BPCellsSubsetArray <- function(x) {
    DelayedArray(BPCellsSubsetSeed(x))
}

#' @export
#' @rdname BPCellsSubset
methods::setClass("BPCellsSubsetMatrix",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsSubsetSeed")
)

#' @importMethodsFrom DelayedArray matrixClass
#' @export
#' @rdname BPCellsSubset
methods::setMethod("matrixClass", "BPCellsSubsetArray", function(x) {
    "BPCellsSubsetMatrix"
})

###################################################################
###########################  Methods  #############################
###################################################################

#' @param object A `BPCellsSubsetSeed` object.
#' @importMethodsFrom DelayedArray path
#' @export
#' @rdname BPCellsSubset
methods::setMethod("path", "BPCellsSubsetSeed", function(object) {
    path(object@matrix)
})

#####################################################
# it's not necessary to re-dispatch the "[" method for `BPCellsSubsetSeed` class
# since the `MatrixSubset` method will use `[` method of `@matrix`. Here, we
# just re-dispatch it to keep consistent.
#' @inheritParams BPCellsMatrix
#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSubset
methods::setMethod(
    "[", "BPCellsSubsetSeed",
    function(x, i, j, ..., drop = FALSE) {
        BPCellsSeed(methods::callNextMethod())
    }
)
