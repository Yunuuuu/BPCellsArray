#' Delayed BPCells TransformRound
#'
#' The `BPCellsTransformRoundArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `TransformRound`
#' object in BPCells.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' `[` (extract methods) of other BPCellsArray objects.
#'
#' @noRd
#' @name BPCellsTransformRound
methods::setClass("BPCellsTransformRoundSeed",
    contains = c("BPCellsTransformedSeed", get_class("TransformRound")),
    slots = list(matrix = "BPCellsSeed")
)

#' @param x A `TransformRound` or `BPCellsTransformRoundSeed` object.
#' @noRd
BPCellsTransformRoundSeed <- function(x) {
    assert_s4_class(x, "TransformRound")
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsTransformRoundSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @noRd
methods::setClass("BPCellsTransformRoundArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsTransformRoundSeed")
)

#' @param seed A `BPCellsTransformRoundSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @rdname internal-methods
methods::setMethod(
    "DelayedArray", "BPCellsTransformRoundSeed",
    function(seed) {
        new_DelayedArray(seed, Class = "BPCellsTransformRoundArray")
    }
)

#' @noRd
BPCellsTransformRoundArray <- function(x) {
    DelayedArray(BPCellsTransformRoundSeed(x))
}

#' @noRd
methods::setClass("BPCellsTransformRound",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsTransformRoundSeed")
)

#' @importMethodsFrom DelayedArray matrixClass
#' @rdname internal-methods
methods::setMethod("matrixClass", "BPCellsTransformRoundArray", function(x) {
    "BPCellsTransformRound"
})

###################################################################
###########################  Methods  #############################
###################################################################
#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "round", "BPCellsSeed", function(x, digits = 0) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "round", "BPCellsMatrix", function(x, digits = 0) {
        DelayedArray(round(x = x, digits = digits))
    }
)
