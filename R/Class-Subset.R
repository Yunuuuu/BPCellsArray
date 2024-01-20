#' Delayed BPCells MatrixSubset
#'
#' The `BPCellsSubsetArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `MatrixSubset` object
#' in BPCells. Usually, you shouldn't use this class directly, instead, you
#' should use `[` (extract methods) of other BPCellsArray objects.
#'
#' @importClassesFrom BPCells MatrixSubset
#' @export
#' @name BPCellsSubset
methods::setClass("BPCellsSubsetSeed",
    contains = c("BPCellsSeed", "MatrixSubset"),
    slots = list(matrix = "BPCellsSeed")
)

#' @param x A `MatrixSubset` or `BPCellsSubsetSeed` object.
#' @export
#' @rdname BPCellsSubset
BPCellsSubsetSeed <- function(x) {
    assert_s4_class(x, "MatrixSubset")
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsSubsetSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsSubset
methods::setClass("BPCellsSubsetArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsSubsetSeed")
)

#' @param seed A `BPCellsSubsetSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @rdname BPCellsSubset
methods::setMethod(
    "DelayedArray", "BPCellsSubsetSeed",
    function(seed) new_DelayedArray(seed, Class = "BPCellsSubsetArray")
)

#' @export
#' @rdname BPCellsSubset
BPCellsSubsetArray <- function(x) {
    DelayedArray(BPCellsSubsetSeed(x))
}

#' @export
#' @rdname BPCellsSubset
methods::setClass("BPCellsSubsetMatrix",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsSubsetSeed")
)

#' @export
#' @importMethodsFrom DelayedArray matrixClass
#' @rdname BPCellsSubset
methods::setMethod("matrixClass", "BPCellsSubsetArray", function(x) {
    "BPCellsSubsetMatrix"
})

###################################################################
###########################  Methods  #############################
###################################################################

#' @export
#' @importMethodsFrom DelayedArray path
#' @rdname BPCellsSubset
methods::setMethod("path", "BPCellsSubsetSeed", function(object) {
    path(object@matrix)
})
