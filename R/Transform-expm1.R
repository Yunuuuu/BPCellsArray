#' Delayed BPCells TransformExpm1
#'
#' The `BPCellsTransformExpm1Array` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `TransformExpm1`
#' object in BPCells.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' `[` (extract methods) of other BPCellsArray objects.
#'
#' @name BPCellsTransformExpm1
#' @noRd
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
#' @noRd
methods::setClass("BPCellsTransformExpm1Array",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsTransformExpm1Seed")
)

#' @param seed A `BPCellsTransformExpm1Seed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @rdname internal-methods
methods::setMethod(
    "DelayedArray", "BPCellsTransformExpm1Seed",
    function(seed) new_DelayedArray(seed, Class = "BPCellsTransformExpm1Array")
)

#' @noRd
BPCellsTransformExpm1Array <- function(x) {
    DelayedArray(BPCellsTransformExpm1Seed(x))
}

#' @noRd
methods::setClass("BPCellsTransformExpm1",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsTransformExpm1Seed")
)

#' @importMethodsFrom DelayedArray matrixClass
#' @rdname internal-methods
methods::setMethod("matrixClass", "BPCellsTransformExpm1Array", function(x) {
    "BPCellsTransformExpm1"
})

###################################################################
###########################  Methods  #############################
###################################################################
#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod("expm1", "BPCellsSeed", function(x) {
    BPCellsSeed(methods::callNextMethod())
})

#' @export
#' @rdname BPCellsMatrix
methods::setMethod("expm1", "BPCellsMatrix", function(x) {
    DelayedArray(expm1(x@seed))
})
