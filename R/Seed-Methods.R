#' House of BPCellsSeed methods
#'
#' Following methods are used by [BPCellsSeed-class] objects, you should always
#' use the methods of [BPCellsMatrix-class]
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
#' @name BPCellsSeed-methods
NULL

#' @importFrom methods show
#' @export
#' @order 1
#' @rdname BPCellsSeed-methods
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
#' @rdname BPCellsSeed-methods
methods::setMethod("type", "BPCellsSeed", function(x) {
    switch(storage_mode(x),
        uint32_t = "integer",
        float = "double",
        double = "double"
    )
})

#' @return
#' - `is_sparse`: Always return `TRUE` for `BPCellsSeed` object.
#' @importFrom DelayedArray is_sparse
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("is_sparse", "BPCellsSeed", function(x) TRUE)

#' @inheritParams S4Arrays::extract_array
#' @return
#' - `extract_array`: A dense matrix.
#' @importFrom DelayedArray extract_array
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod(
    "extract_array", "BPCellsSeed",
    function(x, index) {
        out <- as.matrix(extract_bpcells_array(x, index))
        storage.mode(out) <- type(x)
        out
    }
)

#' @return
#' - `OLD_extract_sparse_array`: A
#'   [SparseArraySeed][DelayedArray::SparseArraySeed-class] object.
#' @importFrom DelayedArray OLD_extract_sparse_array
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod(
    "OLD_extract_sparse_array", "BPCellsSeed",
    function(x, index) {
        methods::as(extract_bpcells_array(x, index), "SparseArraySeed")
    }
)

#' @return
#' - `extract_sparse_array`: A
#'   [SparseArray][SparseArray::SVT_SparseArray-class] object.
#' @importFrom SparseArray extract_sparse_array
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod(
    "extract_sparse_array", "BPCellsSeed",
    function(x, index) {
        methods::as(extract_bpcells_array(x, index), "SparseArray")
    }
)

#' @return
#' - `chunkdim`: `NULL` or the chunk dimensions in an integer vector parallel to
#' `dim(x)`.
#' @importFrom DelayedArray chunkdim
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod(
    "chunkdim", "BPCellsSeed",
    function(x) if (x@transpose) c(1L, ncol(x)) else c(nrow(x), 1L)
)

# t will not change the underlying class
#' @return
#'  - `t`: A [BPCellsSeed] object.
#' @importMethodsFrom BPCells t
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("t", "BPCellsSeed", function(x) methods::callNextMethod())

#' @export
methods::setAs("BPCellsSeed", "matrix", function(from) {
    out <- methods::as(from, "dgCMatrix")
    out <- as.matrix(out)
    if (type(from) == "integer") {
        if (all(out < .Machine$integer.max)) {
            storage.mode(out) <- "integer"
        } else {
            cli::cli_warn(
                "Using double data type since some values exceed {.code .Machine$integer.max}"
            )
        }
    }
    out
})
