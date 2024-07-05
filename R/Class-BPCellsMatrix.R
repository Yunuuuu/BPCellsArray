#' DelayedArray backend of BPCells matrix
#'
#' The `BPCellsMatrix` class just inherits from the
#' [DelayedMatrix][DelayedArray::DelayedMatrix] class.
#'
#' @param ... Additional arguments passed to specific methods
#'  - `aperm`: not used currently.
#'  - `[<-`: arguments passed to
#'    [transpose_storage_order][BPCells::transpose_storage_order]
#' @seealso
#' - [bind][BPCells-bind]: Combine two Objects by Columns or Rows.
#' - [%*%][BPCells-Multiplication]: Matrix Multiplication.
#' - [crossprod][BPCells-crossprod]: Matrix Crossproduct.
#' - [summarization][BPCells-Summarization]: row/col summarization.
#' - [Arith][BPCells-Arithmetic]: Binary Arithmetic operators.
#' - [Math][BPCells-Math]: Math operators.
#' - [Compare][BPCells-Compare]: Compare matrix.
#' - [pmin2/pmax2][pmin2]: Maxima and Minima.
#' - [DelayedArray-utils]: Common operations on DelayedArray objects
#' @aliases BPCellsMatrix-methods
#' @name BPCellsMatrix-class
NULL

#' @param x A `r rd_matrix()`. For `BPCellsArray` and `BPCellsMatrix`
#'    function, a `r rd_seed()` would also be okay.
#' @param object A `r rd_matrix()`.
#' @return
#'  - `BPCellsArray` and `BPCellsMatrix`: A `r rd_matrix()`, since `BPCells` can
#'    only support 2-dim array.
#' @export
#' @rdname BPCellsMatrix-class
BPCellsArray <- function(x) DelayedArray(x)

#' @export
#' @rdname BPCellsMatrix-class
BPCellsMatrix <- BPCellsArray

is_BPCellsArray <- function(x) {
    methods::is(x, "BPCellsArray") || methods::is(x, "BPCellsMatrix")
}

#' @include utils-BPCells.R
#' @export
#' @rdname BPCellsMatrix-class
methods::setClass("BPCellsArray", contains = "DelayedArray")

#' @export
#' @rdname BPCellsMatrix-class
methods::setClass("BPCellsMatrix", contains = "DelayedMatrix")

#' @return
#'  - `matrixClass`: A string, always be `"BPCellsMatrix"`.
#' @importFrom DelayedArray matrixClass
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("matrixClass", "BPCellsArray", function(x) {
    "BPCellsMatrix"
})

.validate_BPCellsArray <- function(object) {
    .validate_seed(object@seed, arg = "@seed")
}

methods::setValidity("BPCellsArray", .validate_BPCellsArray)
methods::setValidity("BPCellsMatrix", .validate_BPCellsArray)

# Since BPCells only support 2-dim matrix, `DelayedArray` will always
# return a `BPCellsMatrix` object.
#' @param seed A [IterableMatrix][BPCellsSeed-class] object.
#' @importFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("DelayedArray", "IterableMatrix", function(seed) {
    seed <- to_DelayedArray(seed) # styler: off
    DelayedArray::new_DelayedArray(seed, Class = "BPCellsArray")
})

# Although `BPCellsDelayedOp` shouldn't be touched by users (Just like the
# ?DelayedOp object) we also define it here
#' @param seed A [BPCellsDelayedOp][BPCellsDelayedOp-class] object.
#' @include Class-Delayed.R
#' @export
#' @rdname internal-methods
methods::setMethod("DelayedArray", "BPCellsDelayedOp", function(seed) {
    seed <- to_BPCells(seed)
    methods::callGeneric()
})

#############################################################
# we won't rely on this function in this package
# this method is provided here in case of user provide a `BPCellsMatrix`
# into BPCellsSeed().
#' @export
#' @rdname internal-methods
methods::setMethod("BPCellsSeed", "BPCellsMatrix", function(x) {
    to_BPCells(x@seed)
})

###################################################################
.show_internal <- function(object) {
    methods::callNextMethod()
    cat("\n")
    cat(sprintf("Storage Data type: %s\n", storage_mode(object)))
    cat(sprintf("Storage axis: %s major\n", storage_axis(object)))

    cat("\n")
    cat("Queued Operations:\n")
    showtree(object@seed)
    invisible(object)
}

#' @importFrom methods show
#' @export
#' @order 1
#' @rdname BPCellsMatrix-class
methods::setMethod("show", "BPCellsArray", .show_internal)

#' @export
#' @order 2
#' @rdname BPCellsMatrix-class
methods::setMethod("show", "BPCellsMatrix", .show_internal)

###########################################################
#' @export
methods::setAs("BPCellsMatrix", "dgCMatrix", function(from) {
    methods::as(to_BPCells(from@seed), "dgCMatrix")
})

# Default drop use `as.array` and `aperm` methods
### S3/S4 combo for aperm.BPCellsMatrix
# list_methods("DelayedAperm")
#' @param a A `r rd_matrix()`.
#' @inheritParams base::aperm
#' @exportS3Method base::aperm
#' @rdname BPCellsMatrix-class
aperm.BPCellsMatrix <- function(a, perm, ...) {
    object <- NextMethod()
    return_BPCellsMatrix(object, .Generic) # nolint
}

#' @importFrom BiocGenerics aperm
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("aperm", "BPCellsMatrix", function(a, perm, ...) {
    object <- methods::callNextMethod()
    return_BPCellsMatrix(object, .Generic) # nolint
})

# when necessary, we restore the BPCellsMatrix, instead of returning the
# DelayedArray object.
return_BPCellsMatrix <- function(object, fn_name) {
    if (!is_BPCellsArray(object)) {
        seed <- object@seed
        if (methods::is(seed, "BPCellsDelayedOp") ||
            methods::is(seed, "IterableMatrix")) {
            return(DelayedArray(seed))
        } else {
            cli::cli_warn(c(
                sprintf(
                    "{.fn %s} method return a {.cls {fclass(object)}} object",
                    fn_name
                ),
                i = "Subsequent operation won't use {.pkg BPCells} methods"
            ))
        }
    }
    object
}

#' @return
#'  - `as.matrix`: A dense matrix.
#' @exportS3Method base::as.matrix
#' @rdname BPCellsMatrix-class
as.matrix.BPCellsMatrix <- function(x) {
    as_matrix_IterableMatrix(to_BPCells(x@seed))
}

#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("as.matrix", "BPCellsMatrix", as.matrix.BPCellsMatrix)

# S3/S4 combo for as.array.BPCellsMatrix
#' @inheritParams BPCellsSeed-class
#' @exportS3Method base::as.array
#' @rdname BPCellsMatrix-class
as.array.BPCellsMatrix <- as.array.BPCellsDelayedOp

#' @return
#'  - `as.array`: A dense matrix or an atomic vector.
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("as.array", "BPCellsMatrix", as.array.BPCellsMatrix)

##########################################################
### Seed contract
###
# https://github.com/Bioconductor/DelayedArray/blob/devel/R/DelayedOp-class.R
# for `dim`, `dimnames`, `extract_array` and `is_sparse` just use the methods
# from `DelayedArray`
#' For BPCellsMatrix object
#' @importMethodsFrom DelayedArray dim
#' @importMethodsFrom DelayedArray dimnames
#' @importMethodsFrom DelayedArray extract_array
#' @importMethodsFrom DelayedArray is_sparse
#' @importMethodsFrom DelayedArray OLD_extract_sparse_array
#' @noRd
NULL

#######################################################
# override methods of DelayedArray
# list_methods("DelayedArray")
#' @return
#' - `t`: A `r rd_matrix()`
#' @importMethodsFrom BPCells t
#' @export
#' @aliases t
#' @rdname BPCellsMatrix-class
methods::setMethod("t", "BPCellsMatrix", function(x) {
    x <- to_BPCells(x@seed)
    ans <- methods::callGeneric()
    DelayedArray(ans)
})

#' @param value
#'  - `type<-`: See the mode argument in [convert_mode].
#'  - `dimnames<-`: A list of dimnames or `NULL`.
#'  - `[<-`: A `r rd_matrix()` or a `r rd_seed()`
#' @return
#' - `type<-`: A `r rd_matrix()` with storage mode converted into the specified.
#' @export
#' @importFrom DelayedArray type<-
#' @rdname BPCellsMatrix-class
methods::setMethod("type<-", "BPCellsMatrix", function(x, value) {
    convert_mode(x, mode = value)
})

#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("is.na", "BPCellsMatrix", function(x) {
    object <- methods::callNextMethod()
    return_BPCellsMatrix(object, .Generic) # nolint
})

#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("is.finite", "BPCellsMatrix", function(x) {
    object <- methods::callNextMethod()
    return_BPCellsMatrix(object, .Generic) # nolint
})

#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("is.infinite", "BPCellsMatrix", function(x) {
    object <- methods::callNextMethod()
    return_BPCellsMatrix(object, .Generic) # nolint
})

#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("is.nan", "BPCellsMatrix", function(x) {
    object <- methods::callNextMethod()
    return_BPCellsMatrix(object, .Generic) # nolint
})

#' @param e1,e2 One of `e1` or `e2` must be a `r rd_matrix()`.
#' @importFrom methods Ops
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("Ops", c("BPCellsArray", "vector"), function(e1, e2) {
    object <- methods::callNextMethod()
    return_BPCellsMatrix(object, .Generic) # nolint
})

#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("Ops", c("vector", "BPCellsArray"), function(e1, e2) {
    object <- methods::callNextMethod()
    return_BPCellsMatrix(object, .Generic) # nolint
})

#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("Ops", c("BPCellsArray", "BPCellsArray"), function(e1, e2) {
    object <- methods::callNextMethod()
    return_BPCellsMatrix(object, .Generic) # nolint
})
