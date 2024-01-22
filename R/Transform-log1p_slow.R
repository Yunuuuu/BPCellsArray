#' Delayed BPCells TransformLog1pSlow
#'
#' The `BPCellsTransformLog1pSlowArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `TransformLog1pSlow`
#' object in BPCells.
#'
#' @param x For Specific functions:
#' - `BPCellsTransformLog1pSlowArray`: A `TransformLog1pSlow` object.
#' - `matrixClass`: A `BPCellsTransformLog1pSlowArray` object.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' [log1p_slow][BPCellsMatrix-class] to create `BPCellsTransformLog1pSlowMatrix`
#' object.
#' @return A `BPCellsTransformLog1pSlowMatrix` object.
#'
#' @name BPCellsTransformLog1pSlow
NULL

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
#' @export 
#' @rdname BPCellsTransformLog1pSlow
methods::setClass("BPCellsTransformLog1pSlowArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsTransformLog1pSlowSeed")
)

#' @param seed A `BPCellsTransformLog1pSlowSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @export 
#' @rdname BPCellsTransformLog1pSlow
methods::setMethod(
    "DelayedArray", "BPCellsTransformLog1pSlowSeed",
    function(seed) new_DelayedArray(seed, Class = "BPCellsTransformLog1pSlowArray")
)

#' @export 
#' @rdname BPCellsTransformLog1pSlow
BPCellsTransformLog1pSlowArray <- function(x) {
    DelayedArray(BPCellsTransformLog1pSlowSeed(x))
}

#' @export 
#' @rdname BPCellsTransformLog1pSlow
methods::setClass("BPCellsTransformLog1pSlowMatrix",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsTransformLog1pSlowSeed")
)

#' @importMethodsFrom DelayedArray matrixClass
#' @export 
#' @rdname BPCellsTransformLog1pSlow
methods::setMethod("matrixClass", "BPCellsTransformLog1pSlowArray", function(x) {
    "BPCellsTransformLog1pSlowMatrix"
})

###################################################################
###########################  Methods  #############################
###################################################################
#' @export
#' @rdname seed-methods
methods::setGeneric("log1p_slow", function(x) {
    makeStandardGeneric("log1p_slow")
})

#' @export
#' @rdname seed-methods
methods::setMethod("log1p_slow", "BPCellsSeed", function(x) {
    BPCellsSeed(BPCells::log1p_slow(x))
})

#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("log1p_slow", "BPCellsMatrix", function(x) {
    DelayedArray(log1p_slow(x@seed))
})
