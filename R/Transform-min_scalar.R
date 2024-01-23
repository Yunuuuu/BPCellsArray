#' Delayed BPCells TransformMin
#'
#' The `BPCellsTransformMinArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `TransformMin`
#' object in BPCells.
#'
#' @param x For Specific functions:
#' - `BPCellsTransformMinArray`: A `TransformMin` object.
#' - `matrixClass`: A `BPCellsTransformMinArray` object.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' [pmin_scalar][BPCellsMatrix-class] to create `BPCellsTransformMinMatrix`
#' object.
#' @return A `BPCellsTransformMinMatrix` object.
#' @name BPCellsTransformMin
NULL

methods::setClass("BPCellsTransformMinSeed",
    contains = c("BPCellsTransformedSeed", get_class("TransformMin"))
)

#' @param x A `TransformMin` or `BPCellsTransformMinSeed` object.
#' @noRd
BPCellsTransformMinSeed <- function(x) {
    assert_s4_class(x, "TransformMin")
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsTransformMinSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsTransformMin
methods::setClass("BPCellsTransformMinArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsTransformMinSeed")
)

#' @param seed A `BPCellsTransformMinSeed` object.
#' @importFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @export
#' @rdname BPCellsTransformMin
methods::setMethod(
    "DelayedArray", "BPCellsTransformMinSeed",
    function(seed) {
        new_DelayedArray(seed, Class = "BPCellsTransformMinArray")
    }
)

#' @export
#' @rdname BPCellsTransformMin
BPCellsTransformMinArray <- function(x) {
    DelayedArray(BPCellsTransformMinSeed(x))
}

#' @export
#' @rdname BPCellsTransformMin
methods::setClass("BPCellsTransformMinMatrix",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsTransformMinSeed")
)

#' @importFrom DelayedArray matrixClass
#' @export
#' @rdname BPCellsTransformMin
methods::setMethod("matrixClass", "BPCellsTransformMinArray", function(x) {
    "BPCellsTransformMinMatrix"
})

###################################################################
###########################  Methods  #############################
###################################################################
#' @export
#' @rdname BPCellsMatrix-class
methods::setGeneric("pmin_scalar", function(object, value) {
    makeStandardGeneric("pmin_scalar")
})

#' @export
#' @rdname seed-methods
methods::setMethod(
    "pmin_scalar", "BPCellsSeed", function(object, value) {
        BPCellsSeed(BPCells::min_scalar(mat = object, val = value))
    }
)

#' @return 
#' - `pmin_scalar`: Take minumum with a global constant
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod(
    "pmin_scalar", "BPCellsMatrix", function(object, value) {
        DelayedArray(pmin_scalar(object = object, value = value))
    }
)
