#' Delayed BPCells TransformPowSlow
#'
#' The `BPCellsTransformPowSlowArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `TransformPowSlow`
#' object in BPCells.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' `[` (extract methods) of other BPCellsArray objects.
#'
#' @noRd
#' @name BPCellsTransformPowSlow
methods::setClass("BPCellsTransformPowSlowSeed",
    contains = c("BPCellsTransformedSeed", get_class("TransformPowSlow")),
    slots = list(matrix = "BPCellsSeed")
)

#' @param x A `TransformPowSlow` or `BPCellsTransformPowSlowSeed` object.
#' @noRd
BPCellsTransformPowSlowSeed <- function(x) {
    assert_s4_class(x, "TransformPowSlow")
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsTransformPowSlowSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @noRd
methods::setClass("BPCellsTransformPowSlowArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsTransformPowSlowSeed")
)

#' @param seed A `BPCellsTransformPowSlowSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @rdname internal-methods
methods::setMethod(
    "DelayedArray", "BPCellsTransformPowSlowSeed",
    function(seed) {
        new_DelayedArray(seed, Class = "BPCellsTransformPowSlowArray")
    }
)

#' @noRd
BPCellsTransformPowSlowArray <- function(x) {
    DelayedArray(BPCellsTransformPowSlowSeed(x))
}

#' @noRd
methods::setClass("BPCellsTransformPowSlow",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsTransformPowSlowSeed")
)

#' @importMethodsFrom DelayedArray matrixClass
#' @rdname internal-methods
methods::setMethod("matrixClass", "BPCellsTransformPowSlowArray", function(x) {
    "BPCellsTransformPowSlow"
})

###################################################################
###########################  Methods  #############################
###################################################################
methods::setGeneric("pow_slow", function(e1, e2) {
    makeStandardGeneric("pow_slow")
})

#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod("pow_slow", "BPCellsSeed", function(e1, e2) {
    BPCellsSeed(BPCells::pow_slow(x = e1, exponent = e2))
})

#' @export
#' @rdname BPCellsMatrix-Class
methods::setMethod("^", c("BPCellsMatrix", "numeric"), function(e1, e2) {
    DelayedArray(pow_slow(e1@seed, e2))
})
