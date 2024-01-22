#' Delayed BPCells TransformPowSlow
#'
#' The `BPCellsTransformPowSlowArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `TransformPowSlow`
#' object in BPCells.
#'
#' @param x For Specific functions:
#' - `BPCellsTransformPowSlowArray`: A `TransformPowSlow` object.
#' - `matrixClass`: A `BPCellsTransformPowSlowArray` object.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' [pow_slow][BPCellsMatrix-class] to create `BPCellsTransformPowSlowMatrix`
#' object.
#' @return A `BPCellsTransformPowSlowMatrix` object.
#' @name BPCellsTransformPowSlow
NULL

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
#' @rdname BPCellsTransformPowSlow
#' @export
methods::setClass("BPCellsTransformPowSlowArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsTransformPowSlowSeed")
)

#' @param seed A `BPCellsTransformPowSlowSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @rdname BPCellsTransformPowSlow
#' @export
methods::setMethod(
    "DelayedArray", "BPCellsTransformPowSlowSeed",
    function(seed) {
        new_DelayedArray(seed, Class = "BPCellsTransformPowSlowArray")
    }
)

#' @rdname BPCellsTransformPowSlow
#' @export
BPCellsTransformPowSlowArray <- function(x) {
    DelayedArray(BPCellsTransformPowSlowSeed(x))
}

#' @rdname BPCellsTransformPowSlow
#' @export
methods::setClass("BPCellsTransformPowSlowMatrix",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsTransformPowSlowSeed")
)

#' @importMethodsFrom DelayedArray matrixClass
#' @rdname BPCellsTransformPowSlow
#' @export
methods::setMethod("matrixClass", "BPCellsTransformPowSlowArray", function(x) {
    "BPCellsTransformPowSlowMatrix"
})

###################################################################
###########################  Methods  #############################
###################################################################
#' @export
#' @rdname BPCellsMatrix-class
methods::setGeneric("pow_slow", function(e1, e2) {
    makeStandardGeneric("pow_slow")
})

#' @inheritParams BPCells-binarize
#' @export
#' @rdname seed-methods
methods::setMethod("pow_slow", "BPCellsSeed", function(e1, e2) {
    BPCellsSeed(BPCells::pow_slow(x = e1, exponent = e2))
})

#' @inheritParams BPCells-binarize
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("^", c("BPCellsMatrix", "numeric"), function(e1, e2) {
    DelayedArray(pow_slow(e1@seed, e2))
})
