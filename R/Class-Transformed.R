#' Delayed BPCells TransformedMatrix
#'
#' The `BPCellsTransformedArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `TransformedMatrix`
#' object in BPCells.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' `[` (extract methods) of other BPCellsArray objects.
#'
#' @param x For Specific functions:
#' - `BPCellsTransformedMatrixArray`: A `TransformedMatrix` object.
#' - `matrixClass`: A `BPCellsTransformedMatrixArray` object.
#' @seealso [BPCellsSeed]
#' @name BPCellsTransformed
NULL

methods::setClass("BPCellsTransformedSeed",
    contains = c("BPCellsSeed", get_class("TransformedMatrix")),
    slots = list(matrix = "BPCellsSeed")
)

#' @param x A `TransformedMatrix` or `BPCellsTransformedSeed` object.
#' @rdname BPCellsTransformed
#' @noRd
BPCellsTransformedSeed <- function(x) {
    assert_s4_class(x, "TransformedMatrix")
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsTransformedSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsTransformed
methods::setClass("BPCellsTransformedArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsTransformedSeed")
)

#' @param seed A `BPCellsTransformedSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @export
#' @rdname BPCellsTransformed
methods::setMethod(
    "DelayedArray", "BPCellsTransformedSeed",
    function(seed) new_DelayedArray(seed, Class = "BPCellsTransformedArray")
)

#' @export
#' @rdname BPCellsTransformed
BPCellsTransformedArray <- function(x) {
    DelayedArray(BPCellsTransformedSeed(x))
}

#' @export
#' @rdname BPCellsTransformed
methods::setClass("BPCellsTransformedMatrix",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsTransformedSeed")
)

#' @importMethodsFrom DelayedArray matrixClass
#' @export
#' @rdname BPCellsTransformed
methods::setMethod("matrixClass", "BPCellsTransformedArray", function(x) {
    "BPCellsTransformedMatrix"
})
