#' Create a Low-level Base Class for Delayed BPCells matrix
#'
#' @param x A BPCells IterableMatrix object.
#' @param ...
#'  - `BPCellsSeed`: Additional arguments passed to specific methods.
#'  - `BPCellsMatrix`: Additional arguments passed to `BPCellsSeed`.
#' @return
#'  - `BPCellsSeed`: A [BPCellsSeed-Class] object.
#'  - `BPCellsMatrix`: A [BPCellsMatrix-Class] object.
#' @name BPCellsSeed
NULL

#' @export
#' @rdname BPCellsSeed
BPCellsMatrix <- function(x, ...) {
    DelayedArray(BPCellsSeed(x = x, ...))
}

#' @export
#' @rdname BPCellsSeed
methods::setGeneric("BPCellsSeed", function(x, ...) {
    standardGeneric("BPCellsSeed")
})

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "BPCellsSeed", function(x, ...) {
    rlang::check_dots_empty()
    x
})

#' @export
#' @rdname internal-methods
methods::setMethod(
    "BPCellsSeed", "Iterable_dgCMatrix_wrapper",
    function(x, ...) {
        rlang::check_dots_empty()
        BPCellsdgCMatrixSeed(x = x)
    }
)

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "MatrixSubset", function(x, ...) {
    rlang::check_dots_empty()
    BPCellsSubsetSeed(x = x)
})

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "ConvertMatrixType", function(x, ...) {
    rlang::check_dots_empty()
    BPCellsConvertSeed(x = x)
})

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "MatrixMultiply", function(x, ...) {
    rlang::check_dots_empty()
    BPCellsMultiplySeed(x = x)
})

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "RenameDims", function(x, ...) {
    rlang::check_dots_empty()
    BPCellsRenameDimsSeed(x = x)
})

#' @export
#' @rdname internal-methods
methods::setMethod("BPCellsSeed", "TransformedMatrix", function(x, ...) {
    rlang::check_dots_empty()
    BPCellsTransformedSeed(x = x)
})

#' @export
#' @rdname internal-methods
methods::setMethod("BPCellsSeed", "TransformLog1p", function(x, ...) {
    rlang::check_dots_empty()
    BPCellsTransformLog1pSeed(x = x)
})

#' @export
#' @rdname internal-methods
methods::setMethod("BPCellsSeed", "TransformLog1pSlow", function(x, ...) {
    rlang::check_dots_empty()
    BPCellsTransformLog1pSlowSeed(x = x)
})

#' @export
#' @rdname internal-methods
methods::setMethod("BPCellsSeed", "TransformExpm1", function(x, ...) {
    rlang::check_dots_empty()
    BPCellsTransformExpm1Seed(x = x)
})

#' @export
#' @rdname internal-methods
methods::setMethod("BPCellsSeed", "TransformExpm1Slow", function(x, ...) {
    rlang::check_dots_empty()
    BPCellsTransformExpm1SlowSeed(x = x)
})

#' @export
#' @rdname internal-methods
methods::setMethod("BPCellsSeed", "TransformSquare", function(x, ...) {
    rlang::check_dots_empty()
    BPCellsTransformSquareSeed(x = x)
})

#' @export
#' @rdname internal-methods
methods::setMethod("BPCellsSeed", "TransformPow", function(x, ...) {
    rlang::check_dots_empty()
    BPCellsTransformPowSeed(x = x)
})

#' @export
#' @rdname internal-methods
methods::setMethod("BPCellsSeed", "TransformPowSlow", function(x, ...) {
    rlang::check_dots_empty()
    BPCellsTransformPowSlowSeed(x = x)
})

#' @export
#' @rdname internal-methods
methods::setMethod("BPCellsSeed", "TransformMin", function(x, ...) {
    rlang::check_dots_empty()
    BPCellsTransformMinSeed(x = x)
})

#' @export
#' @rdname internal-methods
methods::setMethod("BPCellsSeed", "TransformMinByRow", function(x, ...) {
    rlang::check_dots_empty()
    BPCellsTransformMinByRowSeed(x = x)
})

#' @export
#' @rdname internal-methods
methods::setMethod("BPCellsSeed", "TransformMinByCol", function(x, ...) {
    rlang::check_dots_empty()
    BPCellsTransformMinByColSeed(x = x)
})

#' @export
#' @rdname internal-methods
methods::setMethod("BPCellsSeed", "TransformBinarize", function(x, ...) {
    rlang::check_dots_empty()
    BPCellsTransformBinarizeSeed(x = x)
})

#' @export
#' @rdname internal-methods
methods::setMethod("BPCellsSeed", "TransformRound", function(x, ...) {
    rlang::check_dots_empty()
    BPCellsTransformRoundSeed(x = x)
})

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "MatrixRankTransform", function(x, ...) {
    rlang::check_dots_empty()
    BPCellsRankTransformSeed(x = x)
})

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "MatrixMask", function(x, ...) {
    rlang::check_dots_empty()
    BPCellsMaskSeed(x = x)
})

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "ColBindMatrices", function(x, ...) {
    rlang::check_dots_empty()
    BPCellsColBindMatrixSeed(x = x)
})

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "RowBindMatrices", function(x, ...) {
    rlang::check_dots_empty()
    BPCellsRowBindMatrixSeed(x = x)
})

#' @inheritParams BPCells::open_matrix_dir
#' @export
#' @rdname BPCellsSeed
methods::setMethod(
    "BPCellsSeed", "MatrixDir",
    function(x, buffer_size = 8192L) {
        BPCellsDirSeed(x = x, buffer_size = buffer_size)
    }
)
