#' Delayed BPCells TransformSquare
#'
#' The `BPCellsTransformSquareArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `TransformSquare`
#' object in BPCells.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' `[` (extract methods) of other BPCellsArray objects.
#'
#' @noRd
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
#' @noRd
methods::setClass("BPCellsTransformSquareArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsTransformSquareSeed")
)

#' @param seed A `BPCellsTransformSquareSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @noRd
methods::setMethod(
    "DelayedArray", "BPCellsTransformSquareSeed",
    function(seed) {
        new_DelayedArray(seed, Class = "BPCellsTransformSquareArray")
    }
)

#' @noRd
BPCellsTransformSquareArray <- function(x) {
    DelayedArray(BPCellsTransformSquareSeed(x))
}

#' @noRd
methods::setClass("BPCellsTransformSquare",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsTransformSquareSeed")
)

#' @importMethodsFrom DelayedArray matrixClass
#' @noRd
methods::setMethod("matrixClass", "BPCellsTransformSquareArray", function(x) {
    "BPCellsTransformSquare"
})
