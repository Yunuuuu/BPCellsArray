#' Delayed BPCells TransformLog1pSlow
#'
#' The `BPCellsTransformLog1pSlowArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `TransformLog1pSlow`
#' object in BPCells.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' `[` (extract methods) of other BPCellsArray objects.
#'
#' @noRd
#' @name BPCellsTransformLog1pSlow
methods::setClass("BPCellsTransformLog1pSlowSeed",
    contains = c("BPCellsTransformedSeed", get_class("TransformLog1pSlow")),
    slots = list(matrix = "BPCellsSeed")
)

#' @param x A `TransformLog1pSlow` or `BPCellsTransformLog1pSlowSeed` object.
#' @noRd
BPCellsTransformLog1pSlowSeed <- function(x) {
    assert_s4_class(x, "TransformLog1pSlow")
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsTransformLog1pSlowSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @noRd
methods::setClass("BPCellsTransformLog1pSlowArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsTransformLog1pSlowSeed")
)

#' @param seed A `BPCellsTransformLog1pSlowSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @rdname internal-methods
methods::setMethod(
    "DelayedArray", "BPCellsTransformLog1pSlowSeed",
    function(seed) new_DelayedArray(seed, Class = "BPCellsTransformLog1pSlowArray")
)

#' @noRd
BPCellsTransformLog1pSlowArray <- function(x) {
    DelayedArray(BPCellsTransformLog1pSlowSeed(x))
}

#' @noRd
methods::setClass("BPCellsTransformLog1pSlow",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsTransformLog1pSlowSeed")
)

#' @importMethodsFrom DelayedArray matrixClass
#' @rdname internal-methods
methods::setMethod("matrixClass", "BPCellsTransformLog1pSlowArray", function(x) {
    "BPCellsTransformLog1pSlow"
})

###################################################################
###########################  Methods  #############################
###################################################################
methods::setGeneric("log1p_slow", function(x) {
    makeStandardGeneric("log1p_slow")
})

#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod("log1p_slow", "BPCellsSeed", function(x) {
    BPCellsSeed(BPCells::log1p_slow(x))
})

#' @export
#' @rdname BPCellsMatrix-Class
methods::setMethod("log1p_slow", "BPCellsMatrix", function(x) {
    DelayedArray(log1p_slow(x@seed))
})
