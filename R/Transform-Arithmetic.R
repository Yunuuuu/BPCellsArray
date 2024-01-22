#' Delayed BPCells TransformScaleShift
#'
#' The `BPCellsTransformScaleShiftArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `TransformScaleShift`
#' object in BPCells.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' `[` (extract methods) of other BPCellsArray objects.
#'
#' @param x For Specific functions:
#' - `BPCellsTransformScaleShiftArray`: A `TransformScaleShift` object.
#' - `matrixClass`: A `BPCellsTransformScaleShiftArray` object.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' [round][BPCellsMatrix-class] to create `BPCellsTransformScaleShiftMatrix` object.
#'
#' @return A `BPCellsTransformScaleShiftMatrix` object.
#'
#' @name BPCellsTransformScaleShift
NULL

methods::setClass("BPCellsTransformScaleShiftSeed",
    contains = c("BPCellsTransformedSeed", get_class("TransformScaleShift")),
    slots = list(matrix = "BPCellsSeed")
)

#' @param x A `TransformScaleShift` or `BPCellsTransformScaleShiftSeed` object.
#' @noRd
BPCellsTransformScaleShiftSeed <- function(x) {
    assert_s4_class(x, "TransformScaleShift")
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsTransformScaleShiftSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsTransformScaleShift
methods::setClass("BPCellsTransformScaleShiftArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsTransformScaleShiftSeed")
)

#' @param seed A `BPCellsTransformScaleShiftSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @export
#' @rdname BPCellsTransformScaleShift
methods::setMethod(
    "DelayedArray", "BPCellsTransformScaleShiftSeed",
    function(seed) {
        new_DelayedArray(seed, Class = "BPCellsTransformScaleShiftArray")
    }
)

#' @export
#' @rdname BPCellsTransformScaleShift
BPCellsTransformScaleShiftArray <- function(x) {
    DelayedArray(BPCellsTransformScaleShiftSeed(x))
}

#' @export
#' @rdname BPCellsTransformScaleShift
methods::setClass("BPCellsTransformScaleShiftMatrix",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsTransformScaleShiftSeed")
)

#' @importMethodsFrom DelayedArray matrixClass
#' @export
#' @rdname BPCellsTransformScaleShift
methods::setMethod(
    "matrixClass", "BPCellsTransformScaleShiftArray",
    function(x) {
        "BPCellsTransformScaleShiftMatrix"
    }
)
