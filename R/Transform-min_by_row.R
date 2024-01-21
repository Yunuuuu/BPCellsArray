#' Delayed BPCells TransformMinByRow
#'
#' The `BPCellsTransformMinByRowArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `TransformMinByRow`
#' object in BPCells.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' `[` (extract methods) of other BPCellsArray objects.
#'
#' @noRd
#' @name BPCellsTransformMinByRow
methods::setClass("BPCellsTransformMinByRowSeed",
    contains = c("BPCellsTransformedSeed", get_class("TransformMinByRow")),
    slots = list(matrix = "BPCellsSeed")
)

#' @param x A `TransformMinByRow` or `BPCellsTransformMinByRowSeed` object.
#' @noRd
BPCellsTransformMinByRowSeed <- function(x) {
    assert_s4_class(x, "TransformMinByRow")
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsTransformMinByRowSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @noRd
methods::setClass("BPCellsTransformMinByRowArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsTransformMinByRowSeed")
)

#' @param seed A `BPCellsTransformMinByRowSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @noRd
methods::setMethod(
    "DelayedArray", "BPCellsTransformMinByRowSeed",
    function(seed) {
        new_DelayedArray(seed, Class = "BPCellsTransformMinByRowArray")
    }
)

#' @noRd
BPCellsTransformMinByRowArray <- function(x) {
    DelayedArray(BPCellsTransformMinByRowSeed(x))
}

#' @noRd
methods::setClass("BPCellsTransformMinByRow",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsTransformMinByRowSeed")
)

#' @importMethodsFrom DelayedArray matrixClass
#' @noRd
methods::setMethod("matrixClass", "BPCellsTransformMinByRowArray", function(x) {
    "BPCellsTransformMinByRow"
})

###################################################################
###########################  Methods  #############################
###################################################################
methods::setGeneric("pmin_by_row", function(object, values) {
    makeStandardGeneric("pmin_by_row")
})

#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "pmin_by_row", "BPCellsSeed", function(object, values) {
        BPCellsSeed(BPCells::min_by_row(mat = object, vals = values))
    }
)

#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "pmin_by_row", "BPCellsMatrix", function(object, values) {
        DelayedArray(pmin_by_row(mat = object, vals = values))
    }
)
