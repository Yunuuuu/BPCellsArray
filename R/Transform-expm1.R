#' Delayed BPCells TransformExpm1
#'
#' The `BPCellsTransformExpm1Array` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `TransformExpm1`
#' object in BPCells.
#'
#' @param x For Specific functions:
#' - `BPCellsTransformExpm1Array`: A `TransformExpm1` object.
#' - `matrixClass`: A `BPCellsTransformExpm1Array` object.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' [expm1][BPCellsMatrix-class] to create `BPCellsTransformExpm1Matrix`
#' object.
#' @return A `BPCellsTransformExpm1Matrix` object.
#'
#' @name BPCellsTransformExpm1
NULL

methods::setClass("BPCellsTransformExpm1Seed",
    contains = c("BPCellsTransformedSeed", get_class("TransformExpm1")),
    slots = list(matrix = "BPCellsSeed")
)

#' @param x A `TransformExpm1` or `BPCellsTransformExpm1Seed` object.
#' @noRd
BPCellsTransformExpm1Seed <- function(x) {
    assert_s4_class(x, "TransformExpm1")
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsTransformExpm1Seed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsTransformExpm1
methods::setClass("BPCellsTransformExpm1Array",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsTransformExpm1Seed")
)

#' @param seed A `BPCellsTransformExpm1Seed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @export
#' @rdname BPCellsTransformExpm1
methods::setMethod(
    "DelayedArray", "BPCellsTransformExpm1Seed",
    function(seed) new_DelayedArray(seed, Class = "BPCellsTransformExpm1Array")
)

#' @export
#' @rdname BPCellsTransformExpm1
BPCellsTransformExpm1Array <- function(x) {
    DelayedArray(BPCellsTransformExpm1Seed(x))
}

#' @export
#' @rdname BPCellsTransformExpm1
methods::setClass("BPCellsTransformExpm1Matrix",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsTransformExpm1Seed")
)

#' @importMethodsFrom DelayedArray matrixClass
#' @export
#' @rdname BPCellsTransformExpm1
methods::setMethod("matrixClass", "BPCellsTransformExpm1Array", function(x) {
    "BPCellsTransformExpm1Matrix"
})

###################################################################
###########################  Methods  #############################
###################################################################
#' @export
#' @rdname seed-methods
methods::setMethod("expm1", "BPCellsSeed", function(x) {
    BPCellsSeed(methods::callNextMethod())
})

#' @return - `expm1` and `expm1_slow`: compute `exp(x)-1` of matrix.
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("expm1", "BPCellsMatrix", function(x) {
    DelayedArray(expm1(x@seed))
})
