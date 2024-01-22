#' Delayed BPCells TransformMinByRow
#'
#' The `BPCellsTransformMinByRowArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `TransformMinByRow`
#' object in BPCells.
#'
#' @param x For Specific functions:
#' - `BPCellsTransformMinByRowArray`: A `TransformMinByRow` object.
#' - `matrixClass`: A `BPCellsTransformMinByRowArray` object.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' [pmin_by_row][BPCellsMatrix-class] to create `BPCellsTransformMinByRowMatrix`
#' object.
#' @return A `BPCellsTransformMinByRowMatrix` object.
#'
#' @name BPCellsTransformMinByRow
NULL

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
#' @export
#' @rdname BPCellsTransformMinByRow
methods::setClass("BPCellsTransformMinByRowArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsTransformMinByRowSeed")
)

#' @param seed A `BPCellsTransformMinByRowSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @export
#' @rdname BPCellsTransformMinByRow
methods::setMethod(
    "DelayedArray", "BPCellsTransformMinByRowSeed",
    function(seed) {
        new_DelayedArray(seed, Class = "BPCellsTransformMinByRowArray")
    }
)

#' @export
#' @rdname BPCellsTransformMinByRow
BPCellsTransformMinByRowArray <- function(x) {
    DelayedArray(BPCellsTransformMinByRowSeed(x))
}

#' @export
#' @rdname BPCellsTransformMinByRow
methods::setClass("BPCellsTransformMinByRowMatrix",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsTransformMinByRowSeed")
)

#' @importMethodsFrom DelayedArray matrixClass
#' @rdname internal-methods
methods::setMethod("matrixClass", "BPCellsTransformMinByRowArray", function(x) {
    "BPCellsTransformMinByRowMatrix"
})

###################################################################
###########################  Methods  #############################
###################################################################
#' @export
#' @rdname seed-methods
methods::setGeneric("pmin_by_row", function(object, values) {
    makeStandardGeneric("pmin_by_row")
})

#' @export
#' @rdname seed-methods
methods::setMethod(
    "pmin_by_row", "BPCellsSeed", function(object, values) {
        BPCellsSeed(BPCells::min_by_row(mat = object, vals = values))
    }
)

#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod(
    "pmin_by_row", "BPCellsMatrix", function(object, values) {
        DelayedArray(pmin_by_row(object = object, values = values))
    }
)
