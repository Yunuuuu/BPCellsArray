#' Delayed BPCells TransformMin
#'
#' The `BPCellsTransformMinArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `TransformMin`
#' object in BPCells.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' `[` (extract methods) of other BPCellsArray objects.
#'
#' @noRd
#' @name BPCellsTransformMin
methods::setClass("BPCellsTransformMinSeed",
    contains = c("BPCellsTransformedSeed", get_class("TransformMin")),
    slots = list(matrix = "BPCellsSeed")
)

#' @param x A `TransformMin` or `BPCellsTransformMinSeed` object.
#' @noRd
BPCellsTransformMinSeed <- function(x) {
    assert_s4_class(x, "TransformMin")
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsTransformMinSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @noRd
methods::setClass("BPCellsTransformMinArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsTransformMinSeed")
)

#' @param seed A `BPCellsTransformMinSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @noRd
methods::setMethod(
    "DelayedArray", "BPCellsTransformMinSeed",
    function(seed) {
        new_DelayedArray(seed, Class = "BPCellsTransformMinArray")
    }
)

#' @noRd
BPCellsTransformMinArray <- function(x) {
    DelayedArray(BPCellsTransformMinSeed(x))
}

#' @noRd
methods::setClass("BPCellsTransformMin",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsTransformMinSeed")
)

#' @importMethodsFrom DelayedArray matrixClass
#' @noRd
methods::setMethod("matrixClass", "BPCellsTransformMinArray", function(x) {
    "BPCellsTransformMin"
})

###################################################################
###########################  Methods  #############################
###################################################################
methods::setGeneric("pmin_scalar", function(object, value) {
    makeStandardGeneric("pmin_scalar")
})

#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "pmin_scalar", "BPCellsSeed", function(object, value) {
        BPCellsSeed(BPCells::min_scalar(mat = object, val = value))
    }
)

#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "pmin_scalar", "BPCellsMatrix", function(object, value) {
        DelayedArray(pmin_scalar(mat = object, val = value))
    }
)
