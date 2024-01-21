#' Delayed BPCells TransformMinByCol
#'
#' The `BPCellsTransformMinByColArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `TransformMinByCol`
#' object in BPCells.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' `[` (extract methods) of other BPCellsArray objects.
#'
#' @noRd
#' @name BPCellsTransformMinByCol
methods::setClass("BPCellsTransformMinByColSeed",
    contains = c("BPCellsTransformedSeed", get_class("TransformMinByCol")),
    slots = list(matrix = "BPCellsSeed")
)

#' @param x A `TransformMinByCol` or `BPCellsTransformMinByColSeed` object.
#' @noRd
BPCellsTransformMinByColSeed <- function(x) {
    assert_s4_class(x, "TransformMinByCol")
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsTransformMinByColSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @noRd
methods::setClass("BPCellsTransformMinByColArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsTransformMinByColSeed")
)

#' @param seed A `BPCellsTransformMinByColSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @noRd
methods::setMethod(
    "DelayedArray", "BPCellsTransformMinByColSeed",
    function(seed) {
        new_DelayedArray(seed, Class = "BPCellsTransformMinByColArray")
    }
)

#' @noRd
BPCellsTransformMinByColArray <- function(x) {
    DelayedArray(BPCellsTransformMinByColSeed(x))
}

#' @noRd
methods::setClass("BPCellsTransformMinByCol",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsTransformMinByColSeed")
)

#' @importMethodsFrom DelayedArray matrixClass
#' @noRd
methods::setMethod("matrixClass", "BPCellsTransformMinByColArray", function(x) {
    "BPCellsTransformMinByCol"
})

###################################################################
###########################  Methods  #############################
###################################################################
methods::setGeneric("pmin_by_col", function(object, values) {
    makeStandardGeneric("pmin_by_col")
})

#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "pmin_by_col", "BPCellsSeed", function(object, values) {
        BPCellsSeed(BPCells::min_by_col(mat = object, vals = values))
    }
)

#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "pmin_by_col", "BPCellsMatrix", function(object, values) {
        DelayedArray(pmin_by_col(mat = object, vals = values))
    }
)
