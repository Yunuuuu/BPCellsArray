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

#' @importMethodsFrom methods show
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("show", "BPCellsSeed", function(object) {
    show_bpcells(object, "BPCellsSeed", class(object))
})

#' @return
#' - `type`: A string. For all BPCells matrix type of `float` and `double`,
#'   always return `double` since R cannot differentiate 32-bit and 64-bit real
#'   number.
#' @importMethodsFrom DelayedArray type
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
#' @importMethodsFrom DelayedArray is_sparse
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("is_sparse", "BPCellsSeed", function(x) TRUE)

#' @inheritParams S4Arrays::extract_array
#' @return
#' - `extract_array`: A dense matrix.
#' @importMethodsFrom DelayedArray extract_array
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
#' - `extract_sparse_array`: A [SparseArraySeed][DelayedArray::SparseArraySeed]
#'   object.
#' @importMethodsFrom DelayedArray extract_sparse_array
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod(
    "extract_sparse_array", "BPCellsSeed",
    function(x, index) {
        methods::as(extract_bpcells_array(x, index), "SparseArraySeed")
    }
)

#######################################################################
# All delayed operations should be wrapped into a `BPCellsSeed` object
# In BPCells, `dimnames<-` was only defined for `IterableMatrix`.
# `dimnames<-` return another `IterableMatrix` object.
# we wrap it into a `BPCellsSeed` object.
#' @return
#' - `dimnames<-`: A [BPCellsSeed] object, usually a `BPCellsRenameDimsSeed`
#'   object.
#' @export
#' @rdname seed-methods
methods::setMethod(
    "dimnames<-",
    c(x = "BPCellsSeed", value = "list"), function(x, value) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @export
#' @rdname seed-methods
methods::setMethod(
    "dimnames<-",
    c(x = "BPCellsSeed", value = "NULL"), function(x, value) {
        BPCellsSeed(methods::callNextMethod())
    }
)

# t will not change the underlying class
#' @importMethodsFrom DelayedArray t
#' @export
#' @rdname seed-methods
methods::setMethod("t", "BPCellsSeed", function(x) methods::callNextMethod())

# In BPCells, `[<-` was only defined for `IterableMatrix`
#' @inheritParams BPCellsMatrix-class
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
