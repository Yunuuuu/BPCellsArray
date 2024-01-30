#' Base Class for Delayed BPCells matrix
#'
#' The `BPCellsMatrix` class just inherits from the
#' [DelayedMatrix][DelayedArray::DelayedMatrix] class.
#'
#' @slot seed A [BPCellsSeed][BPCellsSeed-class] object.
#' @importClassesFrom DelayedArray DelayedMatrix
#' @name BPCellsMatrix-class
NULL

#' @param x
#'  - `BPCellsArray` and `BPCellsMatrix`: Details see [BPCellsSeed] for
#'    supported object.
#'  - `matrixClass`: A BPCellsArray object.
#' @export
#' @rdname BPCellsMatrix-class
BPCellsArray <- function(x) {
    DelayedArray(BPCellsSeed(x))
}

#' @export
#' @rdname BPCellsMatrix-class
BPCellsMatrix <- BPCellsArray

#' @export
#' @include Class-BPCellsSeed.R
#' @rdname BPCellsMatrix-class
methods::setClass("BPCellsMatrix",
    contains = "DelayedMatrix",
    slots = list(seed = "BPCellsSeed")
)

#' @importClassesFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsMatrix-class
methods::setClass("BPCellsArray",
    contains = "DelayedArray", slots = c(seed = "BPCellsSeed")
)

#' @param seed A `BPCellsSeed` object.
#' @importFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod(
    "DelayedArray", "BPCellsSeed",
    function(seed) new_DelayedArray(seed, Class = "BPCellsArray")
)

#' @importFrom DelayedArray matrixClass
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("matrixClass", "BPCellsArray", function(x) {
    "BPCellsMatrix"
})
