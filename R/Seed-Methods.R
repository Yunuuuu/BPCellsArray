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
#' - [matrix_stats][BPCells-matrix_stats]: Calculate matrix statisticals.
#' @name seed-methods
NULL

#' @importFrom methods show
#' @export
#' @rdname seed-methods
methods::setMethod("show", "BPCellsSeed", function(object) {
    show_bpcells(object, "BPCellsSeed", class(object))
})

#' @return
#' - `type`: A string, indicates the storage type. For all BPCells matrix type
#'   of `float` and `double`, always return `double` since R cannot
#'   differentiate 32-bit and 64-bit real number.
#' @importFrom DelayedArray type
#' @export
#' @rdname seed-methods
methods::setMethod("type", "BPCellsSeed", function(x) {
    switch(BPCells:::matrix_type(x),
        uint32_t = "integer",
        float = "double",
        double = "double"
    )
})

#' @return
#' - `is_sparse`: Always return `TRUE` for `BPCellsSeed` object.
#' @importFrom DelayedArray is_sparse
#' @export
#' @rdname seed-methods
methods::setMethod("is_sparse", "BPCellsSeed", function(x) TRUE)

#' @inheritParams S4Arrays::extract_array
#' @return
#' - `extract_array`: A dense matrix.
#' @importFrom DelayedArray extract_array
#' @export
#' @rdname seed-methods
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
#' @rdname seed-methods
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
#' @rdname seed-methods
methods::setMethod(
    "extract_sparse_array", "BPCellsSeed",
    function(x, index) {
        methods::as(extract_bpcells_array(x, index), "SparseArray")
    }
)

# All delayed operations should be wrapped into a `BPCellsSeed` object
# In BPCells, `dimnames<-` was only defined for `IterableMatrix`.
# `dimnames<-` return another `IterableMatrix` object.
# we wrap it into a `BPCellsSeed` object.
#' @return
#' - `dimnames<-`: A [BPCellsSeed] object, usually a `BPCellsRenameDimsSeed`
#'   object.
#' @importMethodsFrom BPCells dimnames<-
#' @export
#' @rdname seed-methods
methods::setMethod(
    "dimnames<-",
    c(x = "BPCellsSeed", value = "list"), function(x, value) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @importMethodsFrom BPCells dimnames<-
#' @export
#' @rdname seed-methods
methods::setMethod(
    "dimnames<-",
    c(x = "BPCellsSeed", value = "NULL"), function(x, value) {
        BPCellsSeed(methods::callNextMethod())
    }
)

# t will not change the underlying class
#' @return
#'  - `t`: A [BPCellsSeed] object.
#' @importMethodsFrom BPCells t
#' @export
#' @rdname seed-methods
methods::setMethod("t", "BPCellsSeed", function(x) methods::callNextMethod())

# In BPCells, `[<-` was only defined for `IterableMatrix`
#' @return
#' - `[<-`: A [BPCellsSeed] object.
#' @importMethodsFrom BPCells [<-
#' @export
#' @rdname seed-methods
methods::setMethod(
    "[<-", "BPCellsSeed", function(x, i, j, ..., value) {
        BPCellsSeed(methods::callNextMethod())
    }
)
