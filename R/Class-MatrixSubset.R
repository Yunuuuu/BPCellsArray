#' Delayed BPCells MatrixSubset
#'
#' The `BPCellsMatrixSubsetArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `MatrixSubset` object
#' in BPCells. Usually, you shouldn't use this class directly, instead, you
#' should use `[` (extract methods) of other BPCellsArray objects.
#'
#' @importClassesFrom BPCells MatrixSubset
#' @export
#' @name BPCellsMatrixSubset
methods::setClass("BPCellsMatrixSubsetSeed", contains = "MatrixSubset")

#' @param x A `MatrixSubset` or `BPCellsMatrixSubsetSeed` object.
#' @export
#' @rdname BPCellsMatrixSubset
BPCellsMatrixSubsetSeed <- function(x) {
    assert_s4_class(x, "MatrixSubset")
    methods::as(x, "BPCellsMatrixSubsetSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsMatrixSubset
methods::setClass("BPCellsMatrixSubsetArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsMatrixSubsetSeed")
)

#' @param seed A `BPCellsMatrixSubsetSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @rdname BPCellsMatrixSubset
methods::setMethod(
    "DelayedArray", "BPCellsMatrixSubsetSeed",
    function(seed) {
        new_DelayedArray(seed, Class = "BPCellsMatrixSubsetArray")
    }
)

#' @export
#' @rdname BPCellsMatrixSubset
BPCellsMatrixSubsetArray <- function(x) {
    DelayedArray(BPCellsMatrixSubsetSeed(x))
}

#' @export
#' @importClassesFrom DelayedArray DelayedMatrix
#' @rdname BPCellsMatrixSubset
methods::setClass("BPCellsMatrixSubsetMatrix",
    contains = "DelayedMatrix",
    slots = c(seed = "BPCellsMatrixSubsetSeed")
)

#' @export
#' @importMethodsFrom DelayedArray matrixClass
#' @rdname BPCellsMatrixSubset
methods::setMethod("matrixClass", "BPCellsMatrixSubsetArray", function(x) {
    "BPCellsMatrixSubsetMatrix"
})

###################################################################
###########################  Methods  #############################
###################################################################

#' @param object A `BPCellsMatrixSubsetSeed` object.
#' @export
#' @importMethodsFrom methods show
#' @rdname BPCellsMatrixSubset
methods::setMethod("show", "BPCellsMatrixSubsetSeed", function(object) {
    cat(sprintf(
        "%i x %i BPCellsMatrixSubsetSeed object\n",
        nrow(object), ncol(object)
    ))
})

#' @export
#' @importMethodsFrom DelayedArray type
#' @rdname BPCellsMatrixSubset
#' @include utils.R
methods::setMethod("type", "BPCellsMatrixSubsetSeed", bpcells_to_r_type)

#' @export
#' @importMethodsFrom DelayedArray is_sparse
#' @rdname BPCellsMatrixSubset
methods::setMethod(
    "is_sparse", "BPCellsMatrixSubsetSeed",
    function(x) TRUE
)

#' @export
#' @importMethodsFrom DelayedArray path
#' @rdname BPCellsMatrixSubset
methods::setMethod("path", "BPCellsMatrixSubsetSeed", function(object) {
    path(object@matrix)
})

#' @inheritParams S4Arrays::extract_array
#' @export
#' @importMethodsFrom DelayedArray extract_array
#' @rdname BPCellsMatrixSubset
methods::setMethod(
    "extract_array", "BPCellsMatrixSubsetSeed",
    function(x, index) {
        out <- as.matrix(extract_bpcells_array(x, index))
        storage.mode(out) <- type(x)
        out
    }
)

#' @export
#' @importMethodsFrom DelayedArray extract_sparse_array
#' @rdname BPCellsMatrixSubset
methods::setMethod(
    "extract_sparse_array", "BPCellsMatrixSubsetSeed",
    function(x, index) {
        methods::as(extract_bpcells_array(x, index), "SparseArraySeed")
    }
)
