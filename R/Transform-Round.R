#' Delayed BPCells TransformRound
#'
#' The `BPCellsTransformRoundArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `TransformRound`
#' object in BPCells.
#'
#' @param x For Specific functions:
#' - `BPCellsTransformRoundArray`: A `TransformRound` object.
#' - `matrixClass`: A `BPCellsTransformRoundArray` object.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' [round][BPCellsMatrix-class] to create `BPCellsTransformRoundMatrix` object.
#'
#' @return A `BPCellsTransformRoundMatrix` object.
#'
#' @name BPCellsTransformRound
NULL

methods::setClass("BPCellsTransformRoundSeed",
    contains = c("BPCellsTransformedSeed", get_class("TransformRound"))
)

#' @param x A `TransformRound` or `BPCellsTransformRoundSeed` object.
#' @noRd
BPCellsTransformRoundSeed <- function(x) {
    assert_s4_class(x, "TransformRound")
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsTransformRoundSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsTransformRound
methods::setClass("BPCellsTransformRoundArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsTransformRoundSeed")
)

#' @param seed A `BPCellsTransformRoundSeed` object.
#' @importFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @export
#' @rdname BPCellsTransformRound
methods::setMethod(
    "DelayedArray", "BPCellsTransformRoundSeed",
    function(seed) {
        new_DelayedArray(seed, Class = "BPCellsTransformRoundArray")
    }
)

#' @export
#' @rdname BPCellsTransformRound
BPCellsTransformRoundArray <- function(x) {
    DelayedArray(BPCellsTransformRoundSeed(x))
}

#' @export
#' @rdname BPCellsTransformRound
methods::setClass("BPCellsTransformRoundMatrix",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsTransformRoundSeed")
)

#' @importFrom DelayedArray matrixClass
#' @export
#' @rdname BPCellsTransformRound
methods::setMethod("matrixClass", "BPCellsTransformRoundArray", function(x) {
    "BPCellsTransformRoundMatrix"
})

###################################################################
###########################  Methods  #############################
###################################################################

#' @inheritParams BPCellsMatrix-class
#' @export
#' @rdname seed-methods
methods::setMethod(
    "round", "BPCellsSeed", function(x, digits = 0) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @param digits Integer indicating the number of decimal places
#' @return - `round`: Rounding of matrix Numbers.
#' @export
#' @aliases round
#' @rdname BPCellsMatrix-class
methods::setMethod(
    "round", "BPCellsMatrix", function(x, digits = 0) {
        DelayedArray(round(x = x, digits = digits))
    }
)
