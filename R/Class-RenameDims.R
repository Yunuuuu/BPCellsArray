#' Delayed BPCells RenameDims
#'
#' The `BPCellsRenameDimsArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `RenameDims`
#' object in BPCells.
#'
#' @note
#' Usually, you shouldn't use this class directly, instead, you should use
#' [dimnames<-] to create a `BPCellsRenameDimsSeed` object.
#'
#' @export
#' @name BPCellsRenameDims
methods::setClass("BPCellsRenameDimsSeed",
    contains = c("BPCellsSeed", get_class("RenameDims")),
    slots = list(matrix = "BPCellsSeed")
)

#' @param x A `RenameDims` object.
#' @export
#' @rdname BPCellsRenameDims
BPCellsRenameDimsSeed <- function(x) {
    assert_s4_class(x, "RenameDims")
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsRenameDimsSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsRenameDims
methods::setClass("BPCellsRenameDimsArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsRenameDimsSeed")
)

#' @param seed A `BPCellsRenameDimsSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @export
#' @rdname BPCellsRenameDims
methods::setMethod(
    "DelayedArray", "BPCellsRenameDimsSeed",
    function(seed) new_DelayedArray(seed, Class = "BPCellsRenameDimsArray")
)

#' @export
#' @rdname BPCellsRenameDims
BPCellsRenameDimsArray <- function(x) {
    DelayedArray(BPCellsRenameDimsSeed(x))
}

#' @export
#' @rdname BPCellsRenameDims
methods::setClass("BPCellsRenameDimsMatrix",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsRenameDimsSeed")
)

#' @importMethodsFrom DelayedArray matrixClass
#' @export
#' @rdname BPCellsRenameDims
methods::setMethod("matrixClass", "BPCellsRenameDimsArray", function(x) {
    "BPCellsRenameDimsMatrix"
})

###################################################################
###########################  Methods  #############################
###################################################################

#' @param object A `BPCellsRenameDimsSeed` object.
#' @importMethodsFrom DelayedArray path
#' @export
#' @rdname BPCellsRenameDims
methods::setMethod("path", "BPCellsRenameDimsSeed", function(object) {
    path(object@matrix)
})

#' @param ... Ignored, Not used curretly.
#' @inheritParams BPCellsMatrix
#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsRenameDims
methods::setMethod(
    "[", "BPCellsRenameDimsSeed",
    function(x, i, j, ..., drop = FALSE) {
        BPCellsSeed(methods::callNextMethod())
    }
)
