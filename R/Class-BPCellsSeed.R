#' Low-level Base Class for Delayed BPCells matrix
#'
#' The `BPCellsSeed` class just inherits from the `IterableMatrix` object in
#' BPCells package. The purpose for `BPCellsSeed` object is to provide the
#' common methods for all low-level BPCells seed objects. This page only list
#' basic methods for BPCellsSeed object, Please refer to [seed-methods] for
#' full methods list.
#'
#' @param x,object A `BPCellsSeed` object.
#' @export
#' @include utils.R
methods::setClass("BPCellsSeed", contains = get_class("IterableMatrix"))

#' @importFrom methods show
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("show", "BPCellsSeed", function(object) {
    show_bpcells(object, "BPCellsSeed", class(object))
})

#' @return
#' - `type`: A string. For all BPCells matrix type of `float` and `double`,
#'   always return `double` since R cannot differentiate 32-bit and 64-bit real
#'   number.
#' @importFrom DelayedArray type
#' @export
#' @rdname BPCellsSeed-class
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
#' @rdname BPCellsSeed-class
methods::setMethod("is_sparse", "BPCellsSeed", function(x) TRUE)

#' @inheritParams S4Arrays::extract_array
#' @return
#' - `extract_array`: A dense matrix.
#' @importFrom DelayedArray extract_array
#' @export
#' @rdname BPCellsSeed-class
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
#' @rdname BPCellsSeed-class
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
#' @rdname BPCellsSeed-class
methods::setMethod(
    "extract_sparse_array", "BPCellsSeed",
    function(x, index) {
        methods::as(extract_bpcells_array(x, index), "SparseArray")
    }
)

#######################################################################
# All delayed operations should be wrapped into a `BPCellsSeed` object
# In BPCells, `dimnames<-` was only defined for `IterableMatrix`.
# `dimnames<-` return another `IterableMatrix` object.
# we wrap it into a `BPCellsSeed` object.
#' @param value
#'  - `dimnames<-`: A list of dimnames or `NULL`.
#'  - `[<-`: A matrix which can be coerced into
#'     [dgCMatrix][Matrix::dgCMatrix-class].
#' @return
#' - `dimnames<-`: A [BPCellsSeed] object, usually a `BPCellsRenameDimsSeed`
#'   object.
#' @importMethodsFrom BPCells dimnames<-
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod(
    "dimnames<-",
    c(x = "BPCellsSeed", value = "list"), function(x, value) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @importMethodsFrom BPCells dimnames<-
#' @export
#' @rdname BPCellsSeed-class
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
#' @rdname BPCellsSeed-class
methods::setMethod("t", "BPCellsSeed", function(x) methods::callNextMethod())

# In BPCells, `[<-` was only defined for `IterableMatrix`
#' @inheritParams BPCellsMatrix-class
#' @return
#' - `[<-`: A [BPCellsSeed] object.
#' @importMethodsFrom BPCells [<-
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod(
    "[<-", "BPCellsSeed", function(x, i, j, ..., value) {
        BPCellsSeed(methods::callNextMethod())
    }
)
