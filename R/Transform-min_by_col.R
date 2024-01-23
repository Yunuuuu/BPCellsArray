#' Delayed BPCells TransformMinByCol
#'
#' The `BPCellsTransformMinByColArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `TransformMinByCol`
#' object in BPCells.
#'
#' @param x For Specific functions:
#' - `BPCellsTransformMinByColArray`: A `TransformMinByCol` object.
#' - `matrixClass`: A `BPCellsTransformMinByColArray` object.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' [pmin_by_col][BPCellsMatrix-class] to create `BPCellsTransformMinByColMatrix`
#' object.
#' @return A `BPCellsTransformMinByColMatrix` object.
#'
#' @name BPCellsTransformMinByCol
NULL

methods::setClass("BPCellsTransformMinByColSeed",
    contains = c("BPCellsTransformedSeed", get_class("TransformMinByCol"))
)

#' @param x A `TransformMinByCol` or `BPCellsTransformMinByColSeed` object.
#' @noRd
BPCellsTransformMinByColSeed <- function(x) {
    assert_s4_class(x, "TransformMinByCol")
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsTransformMinByColSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsTransformMinByCol
methods::setClass("BPCellsTransformMinByColArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsTransformMinByColSeed")
)

#' @param seed A `BPCellsTransformMinByColSeed` object.
#' @importFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @export
#' @rdname BPCellsTransformMinByCol
methods::setMethod(
    "DelayedArray", "BPCellsTransformMinByColSeed",
    function(seed) {
        new_DelayedArray(seed, Class = "BPCellsTransformMinByColArray")
    }
)

#' @export
#' @rdname BPCellsTransformMinByCol
BPCellsTransformMinByColArray <- function(x) {
    DelayedArray(BPCellsTransformMinByColSeed(x))
}

#' @export
#' @rdname BPCellsTransformMinByCol
methods::setClass("BPCellsTransformMinByColMatrix",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsTransformMinByColSeed")
)

#' @importFrom DelayedArray matrixClass
#' @export
#' @rdname BPCellsTransformMinByCol
methods::setMethod("matrixClass", "BPCellsTransformMinByColArray", function(x) {
    "BPCellsTransformMinByColMatrix"
})

###################################################################
###########################  Methods  #############################
###################################################################
#' @export
#' @rdname BPCellsMatrix-class
methods::setGeneric("pmin_by_col", function(object, values) {
    makeStandardGeneric("pmin_by_col")
})

#' @inheritParams BPCellsMatrix-class
#' @export
#' @rdname seed-methods
methods::setMethod(
    "pmin_by_col", "BPCellsSeed", function(object, values) {
        BPCellsSeed(BPCells::min_by_col(mat = object, vals = values))
    }
)

#' @param values A positive atomic numeric.
#' @return 
#' - `pmin_by_col`: Take the minimum with a per-col constant
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod(
    "pmin_by_col", "BPCellsMatrix", function(object, values) {
        DelayedArray(pmin_by_col(object = object, values = values))
    }
)
