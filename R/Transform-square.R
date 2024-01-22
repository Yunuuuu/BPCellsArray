#' Delayed BPCells TransformSquare
#'
#' The `BPCellsTransformSquareArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `TransformSquare`
#' object in BPCells.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' `^2` of other BPCellsArray objects.
#'
#' @param x For Specific functions:
#' - `BPCellsTransformSquareArray`: A `TransformSquare` object.
#' - `matrixClass`: A `BPCellsTransformSquareArray` object.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' [round][BPCellsMatrix-class] to create `BPCellsTransformSquareMatrix` object.
#'
#' @return A `BPCellsTransformSquareMatrix` object.
#'
#' @name BPCellsTransformSquare
methods::setClass("BPCellsTransformSquareSeed",
    contains = c("BPCellsTransformedSeed", get_class("TransformSquare")),
    slots = list(matrix = "BPCellsSeed")
)

#' @param x A `TransformSquare` or `BPCellsTransformSquareSeed` object.
#' @noRd
BPCellsTransformSquareSeed <- function(x) {
    assert_s4_class(x, "TransformSquare")
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsTransformSquareSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsTransformSquare
methods::setClass("BPCellsTransformSquareArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsTransformSquareSeed")
)

#' @param seed A `BPCellsTransformSquareSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @export
#' @rdname BPCellsTransformSquare
methods::setMethod(
    "DelayedArray", "BPCellsTransformSquareSeed",
    function(seed) {
        new_DelayedArray(seed, Class = "BPCellsTransformSquareArray")
    }
)

#' @export
#' @rdname BPCellsTransformSquare
BPCellsTransformSquareArray <- function(x) {
    DelayedArray(BPCellsTransformSquareSeed(x))
}

#' @export
#' @rdname BPCellsTransformSquare
methods::setClass("BPCellsTransformSquareMatrix",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsTransformSquareSeed")
)

#' @importMethodsFrom DelayedArray matrixClass
#' @export
#' @rdname BPCellsTransformSquare
methods::setMethod("matrixClass", "BPCellsTransformSquareArray", function(x) {
    "BPCellsTransformSquareMatrix"
})
