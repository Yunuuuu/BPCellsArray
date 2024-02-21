#' Low-level Base Class for Delayed BPCells matrix
#'
#' The `BPCellsSeed` class just inherits from the `IterableMatrix` class in
#' `BPCells` package. The purpose for `BPCellsSeed` class is to provide the
#' common methods for all low-level BPCells seed classes.
#'
#' @param x,object A [BPCellsSeed][BPCellsSeed-class] object.
#' @param value
#'  - `dimnames<-`: A list of dimnames or `NULL`.
#'  - `[<-`: A matrix which can be coerced into
#'     [dgCMatrix][Matrix::dgCMatrix-class].
#'  - `pmin_scalar`: Single positive numeric value.
#' @param ... Not used currently.
#' @seealso
#' - [bind][BPCells-bind]: Combine two Objects by Columns or Rows.
#' - [%*%][BPCells-Multiplication]: Matrix Multiplication.
#' - [crossprod][BPCells-crossprod]: Matrix Crossproduct.
#' - [summarization][BPCells-Summarization]: row/col summarization.
#' - [Arithmetic][BPCells-Arithmetic]: Binary Arithmetic operators.
#' - [binarize][BPCells-binarize]: Convert matrix elements to zeros and ones.
#' @include utils.R
#' @aliases BPCellsSeed-methods
#' @export
methods::setClass("BPCellsSeed",
    contains = c(BPCells_class("IterableMatrix"), "VIRTUAL")
)

# BPCells matrix object indeed is a DelayedMatrix, but we regard it as a `seed`
# object in DelayedArray
# we won't use the `DelayedNaryOp` object since it use `seeds` slot to save a
# list of other seed objects while BPCells use left (and right) or matrix_list.

methods::setValidity("BPCellsSeed", function(object) {
    if (length(dim(object)) != 2L) {
        cli::cli_abort("{.pkg BPCells} can only support 2-dim matrix")
    }
    TRUE
})

methods::setClass("BPCellsBasicSeed", contains = c("BPCellsSeed", "VIRTUAL"))
methods::setClass("BPCellsUnaryOpsSeed", contains = c("BPCellsSeed", "VIRTUAL"))
methods::setClass("BPCellsNaryOpsSeed", contains = c("BPCellsSeed", "VIRTUAL"))

#############################################################
# used to extract the actual entity of `BPCellsSeed` objet.
methods::setGeneric("entity", function(x, ...) standardGeneric("entity"))
methods::setMethod("entity", "BPCellsBasicSeed", function(x) x)
methods::setMethod("entity", "BPCellsUnaryOpsSeed", function(x) x@matrix)

# nary_seeds <- c(
#     "BPCellsBindMatrixSeed", "BPCellsMaskSeed",
#     "BPCellsMultiplySeed"
# )
# unary_seeds <- c(
#     "BPCellsConvertSeed", "BPCellsRankTransformSeed",
#     "BPCellsRenameDimsSeed", "BPCellsSubsetSeed", "BPCellsTransformedSeed"
# )

##################################################################
#' @importFrom methods show
#' @export
#' @order 1
#' @rdname BPCellsSeed-class
methods::setMethod("show", "BPCellsSeed", function(object) {
    show_bpcells(object, "BPCellsSeed", class(object))
})

#' @return
#' - `type`: A string, indicates the storage type. For all BPCells matrix type
#'   of `float` and `double`, always return `double` since R cannot
#'   differentiate 32-bit and 64-bit real number. See
#'   [storage_mode][convert_mode].
#' @importFrom DelayedArray type
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("type", "BPCellsSeed", function(x) {
    mode_to_type(storage_mode(x))
})

#' @return
#' - `is_sparse`: Always return `TRUE` for `BPCellsSeed` object.
#' @importFrom DelayedArray is_sparse
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("is_sparse", "BPCellsSeed", function(x) TRUE)

###############################################
#' @export
methods::setAs(
    "BPCellsSeed", "dgCMatrix",
    function(from) methods::callNextMethod()
)

#' @importMethodsFrom DelayedArray drop
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("drop", "BPCellsSeed", drop_internal)

# S3/S4 combo for as.array.BPCellsSeed
#' @exportS3Method base::as.array
#' @rdname BPCellsSeed-class
as.array.BPCellsSeed <- function(x, drop = FALSE) {
    assert_bool(drop)
    matrix <- as.matrix(x)
    if (drop) drop(matrix) else matrix
}

#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("as.array", "BPCellsSeed", as.array.BPCellsSeed)

#' @exportS3Method base::as.matrix
#' @rdname BPCellsSeed-class
as.matrix.BPCellsSeed <- function(x) {
    mat <- as.matrix(methods::as(x, "dgCMatrix")) # always be numeric mode
    if (type(x) == "integer") {
        mat <- matrix_to_integer(mat)
    }
    mat
}

#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("as.matrix", "BPCellsSeed", as.matrix.BPCellsSeed)

#' @export
methods::setAs("ANY", "BPCellsSeed", function(from) {
    BPCellsSeed(from)
})

###############################################
#' @inheritParams S4Arrays::extract_array
#' @return
#' - `extract_array`: A dense matrix.
#' @importFrom DelayedArray extract_array
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod(
    "extract_array", "BPCellsSeed",
    function(x, index) {
        slice <- S4Arrays:::subset_by_Nindex(x, index)
        as.matrix(slice)
    }
)

extract_dgCMatrix <- function(x, index) {
    slice <- S4Arrays:::subset_by_Nindex(x, index)
    methods::as(slice, "dgCMatrix")
}

#' @return
#' - `OLD_extract_sparse_array`: A
#'   [SparseArraySeed][DelayedArray::SparseArraySeed-class] object.
#' @importFrom DelayedArray OLD_extract_sparse_array
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod(
    "OLD_extract_sparse_array", "BPCellsSeed",
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
    "extract_sparse_array", "BPCellsSeed",
    function(x, index) {
        methods::as(extract_dgCMatrix(x, index), "SparseArray")
    }
)

#' @return
#' - `chunkdim`: the chunk dimensions in an integer vector parallel to `dim(x)`.
#' @importFrom DelayedArray chunkdim
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod(
    "chunkdim", "BPCellsSeed",
    function(x) if (x@transpose) c(1L, ncol(x)) else c(nrow(x), 1L)
)

# t will not change the underlying class
#' @return
#'  - `t`: A [BPCellsSeed] object.
#' @importMethodsFrom BPCells t
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("t", "BPCellsSeed", function(x) methods::callNextMethod())
