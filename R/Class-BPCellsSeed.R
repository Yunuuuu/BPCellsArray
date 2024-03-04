#' `IterableMatrix` object methods
#'
#' @param x A `IterableMatrix` object.
#' @aliases IterableMatrix
#' @name BPCellsSeed-class
NULL

# nary_seeds <- c(
#     "BPCellsBindMatrixSeed", "BPCellsMaskSeed",
#     "BPCellsMultiplySeed"
# )
# unary_seeds <- c(
#     "BPCellsConvertSeed", "BPCellsRankTransformSeed",
#     "BPCellsRenameDimsSeed", "BPCellsSubsetSeed", "BPCellsTransformedSeed"
# )

###########################################################
# Seed Contract
#' @return
#' - `type`: A string, indicates the storage type. For all BPCells matrix type
#'   of `float` and `double`, always return `double` since R cannot
#'   differentiate 32-bit and 64-bit real number. See
#'   [storage_mode][convert_mode].
#' @importFrom DelayedArray type
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("type", "IterableMatrix", function(x) {
    switch(storage_mode(x),
        uint32_t = "integer",
        float = ,
        double = "double"
    )
})

subset_IterableMatrix <- function(x, Nindex) {
    # `subset_by_Nindex` will just use the `[` method.
    # `[.IterableMatrix` don't support `drop` argument
    S4Arrays:::subset_by_Nindex(x, Nindex = Nindex, drop = FALSE)
}

# respect `type(x)`
as_matrix_IterableMatrix <- function(x) {
    # `mat` will always be numeric mode
    mat <- as.matrix(methods::as(x, "dgCMatrix"))
    # to keep the original mode, we transform it when necessary
    if (storage_mode(x) == "uint32_t") {
        mat <- matrix_to_integer(mat)
    }
    mat
}

# S3/S4 combo for as.array.IterableMatrix
#' @param drop A bool, if `TRUE`, any extents of length one will be removed and
#'  return an atomic vector.
#' @exportS3Method base::as.array
#' @rdname BPCellsSeed-class
as.array.IterableMatrix <- function(x, drop = FALSE) {
    assert_bool(drop)
    mat <- extract_array(x, vector("list", length = length(dim(x))))
    if (drop) drop(mat) else mat
}

#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("as.array", "IterableMatrix", as.array.IterableMatrix)

#' @inheritParams S4Arrays::extract_array
#' @return
#' - `extract_array`: A dense matrix.
#' @importFrom DelayedArray extract_array
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("extract_array", "IterableMatrix", function(x, index) {
    slice <- subset_IterableMatrix(x, index)
    # `IterableMatrix` only support 2-dimention
    as_matrix_IterableMatrix(slice)
})

extract_dgCMatrix <- function(x, index) {
    slice <- subset_IterableMatrix(x, index)
    methods::as(slice, "dgCMatrix")
}

#' @return
#' - `OLD_extract_sparse_array`: A
#'   [SparseArraySeed][DelayedArray::SparseArraySeed-class] object.
#' @importFrom DelayedArray OLD_extract_sparse_array
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod(
    "OLD_extract_sparse_array", "IterableMatrix",
    function(x, index) {
        methods::as(extract_dgCMatrix(x, index), "SparseArraySeed")
    }
)

#' @return
#' - `extract_sparse_array`: A
#'   [SparseArray][SparseArray::SVT_SparseArray-class] object.
#' @importFrom SparseArray extract_sparse_array
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod(
    "extract_sparse_array", "IterableMatrix",
    function(x, index) {
        methods::as(extract_dgCMatrix(x, index), "SparseArray")
    }
)

#' @return
#' - `is_sparse`: Always return `TRUE`.
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("is_sparse", "IterableMatrix", function(x) TRUE)

#' @return
#' - `chunkdim`: the chunk dimensions in an integer vector parallel to `dim(x)`.
#' @importFrom DelayedArray chunkdim
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod(
    "chunkdim", "IterableMatrix",
    function(x) if (x@transpose) c(1L, ncol(x)) else c(nrow(x), 1L)
)
