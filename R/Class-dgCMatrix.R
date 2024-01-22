#' Wrap Class of `Iterable_dgCMatrix_wrapper`
#'
#' @note Usually, you shouldn't use this class directly.
#'
#' @param x For Specific functions:
#' - `BPCellsdgCmatrixArray`: A `Iterable_dgCMatrix_wrapper` object.
#' - `matrixClass`: A `BPCellsdgCmatrixArray` object.
#' @seealso [BPCellsSeed]
#' @name BPCellsdgCMatrix
#' @noRd
NULL

methods::setClass("BPCellsdgCMatrixSeed",
    contains = c("BPCellsSeed", get_class("Iterable_dgCMatrix_wrapper"))
)

#' @param x A `Iterable_dgCMatrix_wrapper` object.
#' @noRd
BPCellsdgCMatrixSeed <- function(x) {
    assert_s4_class(x, "Iterable_dgCMatrix_wrapper")
    methods::as(x, "BPCellsdgCMatrixSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @noRd
methods::setClass("BPCellsdgCMatrixArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsdgCMatrixSeed")
)

#' @param seed A `BPCellsdgCMatrixSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @rdname internal-methods
methods::setMethod(
    "DelayedArray", "BPCellsdgCMatrixSeed",
    function(seed) new_DelayedArray(seed, Class = "BPCellsdgCMatrixArray")
)

#' @importClassesFrom DelayedArray DelayedMatrix
#' @noRd
methods::setClass("BPCellsdgCMatrixMatrix",
    contains = "DelayedMatrix",
    slots = c(seed = "BPCellsdgCMatrixSeed")
)

#' @importMethodsFrom DelayedArray matrixClass
#' @rdname internal-methods
methods::setMethod("matrixClass", "BPCellsdgCMatrixArray", function(x) {
    "BPCellsdgCMatrixMatrix"
})
