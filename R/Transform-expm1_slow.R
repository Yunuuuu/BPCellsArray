#' Delayed BPCells TransformExpm1Slow
#'
#' The `BPCellsTransformExpm1SlowArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `TransformExpm1Slow`
#' object in BPCells.
#'
#' @param x For Specific functions:
#' - `BPCellsTransformExpm1SlowArray`: A `TransformExpm1Slow` object.
#' - `matrixClass`: A `BPCellsTransformExpm1SlowArray` object.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' [expm1_slow][BPCellsMatrix-class] to create `BPCellsTransformExpm1SlowMatrix`
#' object.
#'
#' @name BPCellsTransformExpm1Slow
NULL

methods::setClass("BPCellsTransformExpm1SlowSeed",
    contains = c("BPCellsTransformedSeed", get_class("TransformExpm1Slow"))
)

#' @param x A `TransformExpm1Slow` or `BPCellsTransformExpm1SlowSeed` object.
#' @noRd
BPCellsTransformExpm1SlowSeed <- function(x) {
    assert_s4_class(x, "TransformExpm1Slow")
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsTransformExpm1SlowSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsTransformExpm1Slow
methods::setClass("BPCellsTransformExpm1SlowArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsTransformExpm1SlowSeed")
)

#' @param seed A `BPCellsTransformExpm1SlowSeed` object.
#' @importFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @export
#' @rdname BPCellsTransformExpm1Slow
methods::setMethod(
    "DelayedArray", "BPCellsTransformExpm1SlowSeed",
    function(seed) {
        new_DelayedArray(seed, Class = "BPCellsTransformExpm1SlowArray")
    }
)

#' @export
#' @rdname BPCellsTransformExpm1Slow
BPCellsTransformExpm1SlowArray <- function(x) {
    DelayedArray(BPCellsTransformExpm1SlowSeed(x))
}

#' @export
#' @rdname BPCellsTransformExpm1Slow
methods::setClass("BPCellsTransformExpm1SlowMatrix",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsTransformExpm1SlowSeed")
)

#' @importFrom DelayedArray matrixClass
#' @export
#' @rdname BPCellsTransformExpm1Slow
methods::setMethod("matrixClass", "BPCellsTransformExpm1SlowArray", function(x) {
    "BPCellsTransformExpm1SlowMatrix"
})

###################################################################
###########################  Methods  #############################
###################################################################
#' @export
#' @rdname BPCellsMatrix-class
methods::setGeneric("expm1_slow", function(x) {
    makeStandardGeneric("expm1_slow")
})

#' @export
#' @rdname seed-methods
methods::setMethod("expm1_slow", "BPCellsSeed", function(x) {
    BPCellsSeed(BPCells::expm1_slow(x))
})

#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("expm1_slow", "BPCellsMatrix", function(x) {
    DelayedArray(expm1_slow(x@seed))
})
