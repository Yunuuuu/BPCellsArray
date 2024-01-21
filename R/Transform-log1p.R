#' Delayed BPCells TransformLog1p
#'
#' The `BPCellsTransformLog1pArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `TransformLog1p`
#' object in BPCells.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' `[` (extract methods) of other BPCellsArray objects.
#'
#' @noRd
#' @name BPCellsTransformLog1p
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
#' @noRd
methods::setClass("BPCellsTransformLog1pArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsTransformLog1pSeed")
)

#' @param seed A `BPCellsTransformLog1pSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @rdname internal-methods
methods::setMethod(
    "DelayedArray", "BPCellsTransformLog1pSeed",
    function(seed) new_DelayedArray(seed, Class = "BPCellsTransformLog1pArray")
)

#' @noRd
BPCellsTransformLog1pArray <- function(x) {
    DelayedArray(BPCellsTransformLog1pSeed(x))
}

#' @noRd
methods::setClass("BPCellsTransformLog1p",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsTransformLog1pSeed")
)

#' @importMethodsFrom DelayedArray matrixClass
#' @rdname internal-methods
methods::setMethod("matrixClass", "BPCellsTransformLog1pArray", function(x) {
    "BPCellsTransformLog1p"
})

###################################################################
###########################  Methods  #############################
###################################################################
#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod("log1p", "BPCellsSeed", function(x) {
    BPCellsSeed(methods::callNextMethod())
})

#' @export
#' @rdname BPCellsMatrix-Class
methods::setMethod("log1p", "BPCellsMatrix", function(x) {
    DelayedArray(log1p(x@seed))
})
