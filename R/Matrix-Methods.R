#' Basic operations for `BPCellsMatrix` object
#' 
#' @param x,object A `BPCellsMatrix` object.
#' @inheritParams seed-methods
#' @inherit seed-methods seealso
#' @name BPCellsMatrix-methods
NULL

#' @importFrom methods show
#' @export
#' @rdname BPCellsMatrix-methods
methods::setMethod("show", "BPCellsMatrix", function(object) {
    show_bpcells(object@seed, "DelayedMatrix", class(object))
})

#' @return
#' - `t`: A [BPCellsMatrix] object.
#' @importMethodsFrom BPCells t
#' @export
#' @rdname BPCellsMatrix-methods
methods::setMethod("t", "BPCellsMatrix", function(x) {
    DelayedArray(t(x@seed))
})

#' @inheritParams seed-methods
#' @return
#' - `[`: A [BPCellsMatrix] object.
#' @export
#' @rdname BPCellsMatrix-methods
methods::setMethod(
    "[", c("BPCellsMatrix", "ANY", "ANY"),
    function(x, i, j, ..., drop = FALSE) {
        DelayedArray(x@seed[i, j, ...])
    }
)

#' @inheritParams seed-methods
#' @export
#' @rdname internal-methods
methods::setMethod(
    "[", c("BPCellsMatrix", "missing", "ANY"),
    function(x, i, j, ..., drop = FALSE) {
        DelayedArray(x@seed[, j, ...])
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "[", c("BPCellsMatrix", "ANY", "missing"),
    function(x, i, j, ..., drop = FALSE) {
        DelayedArray(x@seed[i, , ...])
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "[", c("BPCellsMatrix", "missing", "missing"),
    function(x, i, j, ..., drop = FALSE) x
)

#' @return
#' - `[<-`: A [BPCellsMatrix] object.
#' @export
#' @rdname BPCellsMatrix-methods
methods::setMethod(
    "[<-", c("BPCellsMatrix", "ANY", "ANY"),
    function(x, i, j, ..., value) {
        seed <- x@seed
        seed[i, j, ...] <- value
        DelayedArray(seed)
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "[<-", c("BPCellsMatrix", "missing", "ANY"),
    function(x, i, j, ..., value) {
        seed <- x@seed
        seed[, j, ...] <- value
        DelayedArray(seed)
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "[<-", c("BPCellsMatrix", "ANY", "missing"),
    function(x, i, j, ..., value) {
        seed <- x@seed
        seed[i, , ...] <- value
        DelayedArray(seed)
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "[<-", c("BPCellsMatrix", "missing", "missing"),
    function(x, i, j, ..., value) {
        seed <- x@seed
        seed[, , ...] <- value
        DelayedArray(seed)
    }
)

methods::setClassUnion("ListOrNULL", c("list", "NULL"))

# for `dim`, `dimnames`, `extract_array` and `is_sparse` just use the methods
# from DelayedArray
#' For BPCellsMatrix object
#' @importMethodsFrom DelayedArray dim
#' @importMethodsFrom DelayedArray extract_array
#' @importMethodsFrom DelayedArray is_sparse
#' @importMethodsFrom DelayedArray OLD_extract_sparse_array
#' @noRd
NULL

#' @return
#' - `dimnames<-`: A [BPCellsMatrix][BPCellsMatrix] object.
#' @importMethodsFrom BPCells dimnames<-
#' @export
#' @rdname BPCellsMatrix-methods
methods::setMethod(
    "dimnames<-",
    c(x = "BPCellsMatrix", value = "ListOrNULL"), function(x, value) {
        seed <- x@seed
        dimnames(seed) <- value
        DelayedArray(seed)
    }
)
