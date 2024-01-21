#' Delayed BPCells TransformExpm1Slow
#'
#' The `BPCellsTransformExpm1SlowArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `TransformExpm1Slow`
#' object in BPCells.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' `[` (extract methods) of other BPCellsArray objects.
#'
#' @name BPCellsTransformExpm1Slow
#' @noRd
methods::setClass("BPCellsTransformExpm1SlowSeed",
    contains = c("BPCellsTransformedSeed", get_class("TransformExpm1Slow")),
    slots = list(matrix = "BPCellsSeed")
)

#' @param x A `TransformExpm1Slow` or `BPCellsTransformExpm1SlowSeed` object.
#' @noRd
BPCellsTransformExpm1SlowSeed <- function(x) {
    assert_s4_class(x, "TransformExpm1Slow")
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsTransformExpm1SlowSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @noRd
methods::setClass("BPCellsTransformExpm1SlowArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsTransformExpm1SlowSeed")
)

#' @param seed A `BPCellsTransformExpm1SlowSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @rdname internal-methods
methods::setMethod(
    "DelayedArray", "BPCellsTransformExpm1SlowSeed",
    function(seed) {
        new_DelayedArray(seed, Class = "BPCellsTransformExpm1SlowArray")
    }
)

#' @noRd
BPCellsTransformExpm1SlowArray <- function(x) {
    DelayedArray(BPCellsTransformExpm1SlowSeed(x))
}

#' @noRd
methods::setClass("BPCellsTransformExpm1Slow",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsTransformExpm1SlowSeed")
)

#' @importMethodsFrom DelayedArray matrixClass
#' @rdname internal-methods
methods::setMethod("matrixClass", "BPCellsTransformExpm1SlowArray", function(x) {
    "BPCellsTransformExpm1Slow"
})

###################################################################
###########################  Methods  #############################
###################################################################
methods::setGeneric("expm1_slow", function(x) {
    makeStandardGeneric("expm1_slow")
})

#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod("expm1_slow", "BPCellsSeed", function(x) {
    BPCellsSeed(BPCells::expm1_slow(x))
})

#' @export
#' @rdname BPCellsMatrix-Class
methods::setMethod("expm1_slow", "BPCellsMatrix", function(x) {
    DelayedArray(expm1_slow(x@seed))
})
