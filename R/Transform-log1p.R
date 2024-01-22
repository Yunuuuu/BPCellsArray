#' Delayed BPCells TransformLog1p
#'
#' The `BPCellsTransformLog1pArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `TransformLog1p`
#' object in BPCells.
#'
#' @param x For Specific functions:
#' - `BPCellsTransformLog1pArray`: A `TransformLog1p` object.
#' - `matrixClass`: A `BPCellsTransformLog1pArray` object.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' [log1p][BPCellsMatrix-class] to create `BPCellsTransformLog1pMatrix`
#' object.
#' @return A `BPCellsTransformLog1pMatrix` object.
#' @name BPCellsTransformLog1p
NULL

methods::setClass("BPCellsTransformLog1pSeed",
    contains = c("BPCellsTransformedSeed", get_class("TransformLog1p")),
    slots = list(matrix = "BPCellsSeed")
)

#' @param x A `TransformLog1p` or `BPCellsTransformLog1pSeed` object.
#' @noRd
BPCellsTransformLog1pSeed <- function(x) {
    assert_s4_class(x, "TransformLog1p")
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsTransformLog1pSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @export 
#' @rdname BPCellsTransformLog1p
methods::setClass("BPCellsTransformLog1pArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsTransformLog1pSeed")
)

#' @param seed A `BPCellsTransformLog1pSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @export 
#' @rdname BPCellsTransformLog1p
methods::setMethod(
    "DelayedArray", "BPCellsTransformLog1pSeed",
    function(seed) new_DelayedArray(seed, Class = "BPCellsTransformLog1pArray")
)

#' @export 
#' @rdname BPCellsTransformLog1p
BPCellsTransformLog1pArray <- function(x) {
    DelayedArray(BPCellsTransformLog1pSeed(x))
}

#' @export 
#' @rdname BPCellsTransformLog1p
methods::setClass("BPCellsTransformLog1pMatrix",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsTransformLog1pSeed")
)

#' @importMethodsFrom DelayedArray matrixClass
#' @export 
#' @rdname BPCellsTransformLog1p
methods::setMethod("matrixClass", "BPCellsTransformLog1pArray", function(x) {
    "BPCellsTransformLog1pMatrix"
})

###################################################################
###########################  Methods  #############################
###################################################################
#' @export
#' @rdname seed-methods
methods::setMethod("log1p", "BPCellsSeed", function(x) {
    BPCellsSeed(methods::callNextMethod())
})

#' @return 
#'  - `log1p` and `log1p_slow`: compute `log(1+x)` of matrix.
#' @export
#' @aliases log1p
#' @rdname BPCellsMatrix-class
methods::setMethod("log1p", "BPCellsMatrix", function(x) {
    DelayedArray(log1p(x@seed))
})
