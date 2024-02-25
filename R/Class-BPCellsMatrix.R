#' DelayedArray backend of BPCells matrix
#'
#' The `BPCellsMatrix` class just inherits from the
#' [DelayedMatrix][DelayedArray::DelayedMatrix] class.
#'
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
#' @inherit BPCellsSeed-class seealso
#' @name BPCellsMatrix-class
NULL


#' @param delayed A bool value. If `TRUE`, will convert `IterableMatrix` into a
#'   parallel [DelayedOp][DelayedArray::DelayedOp-class] object
#'   (`BPCellsDelayedOp`), if `FALSE`, will use the `IterableMatrix` object as
#'   the seed directly. see [set_delayed] for details.
#' @return
#'  - `BPCellsArray` and `BPCellsMatrix`: A `BPCellsMatrix` object, since
#'    `BPCells` can only support 2-dim array.
#' @param x,object A [BPCellsMatrix][BPCellsMatrix-class] object
#' @export
#' @rdname BPCellsMatrix-class
BPCellsArray <- function(x, delayed = NULL) {
    assert_bool(delayed, null_ok = TRUE)
    delayed <- delayed %||% GlobalOptions$DelayedBPCells
    with_delayed(delayed, DelayedArray(BPCellsSeed(x)))
}

#' @export
#' @rdname BPCellsMatrix-class
BPCellsMatrix <- BPCellsArray

#' @export
#' @rdname BPCellsMatrix-class
methods::setClass("BPCellsArray",
    contains = "DelayedArray",
    slots = list(delayed = "logical"),
    prototype = list(delayed = GlobalOptions$DelayedBPCells)
)

#' @export
#' @rdname BPCellsMatrix-class
methods::setClass("BPCellsMatrix",
    contains = "DelayedMatrix",
    slots = list(delayed = "logical"),
    prototype = list(delayed = GlobalOptions$DelayedBPCells)
)

#' @return
#'  - `matrixClass`: A string, always be `"BPCellsMatrix"`.
#' @importFrom DelayedArray matrixClass
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("matrixClass", "BPCellsArray", function(x) {
    "BPCellsMatrix"
})

.validate_delayed <- function(delayed, arg = rlang::caller_arg(delayed)) {
    msg <- "{.arg {arg}} must be a single bool value"
    if (length(delayed) != 1L) {
        cli::cli_abort(
            c(msg, i = "You have provided a length {length(delayed)}")
        )
    } else if (is.na(delayed)) {
        cli::cli_abort(c(msg, i = "{.code NA} is not allowed"))
    }
    return(TRUE)
}

.validate_BPCellsArray <- function(object) {
    .validate_seed(object@seed, arg = "@seed")
    .validate_delayed(object@delayed, arg = "@delayed")
}

methods::setValidity("BPCellsArray", .validate_BPCellsArray)
methods::setValidity("BPCellsMatrix", .validate_BPCellsArray)

#' Since BPCells only support 2-dim matrix, `new_BPCellsArray` will always
#' return a `BPCellsMatrix` object.
#' @importFrom DelayedArray new_DelayedArray
#' @noRd
NULL

#' @param seed A [IterableMatrix][BPCellsSeed-class] or
#' [BPCellsDelayedOp][BPCellsSeed-class] object.
#' @importFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("DelayedArray", "IterableMatrix", function(seed) {
    # DelayedArray can only accept one argument,
    # so we always use with_delayed with DelayedArray to
    # control the delayed behaviour and just assign the delayed
    # value into the `BPCellsArray` object after creating `BPCellsArray` object
    delayed <- GlobalOptions$DelayedBPCells
    object <- new_DelayedArray(
        to_DelayedArray(seed, delayed = delayed),
        Class = "BPCellsArray"
    )
    object@delayed <- delayed
    object
})

#' @include Class-Delayed.R
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("DelayedArray", "BPCellsDelayedOp", function(seed) {
    object <- new_DelayedArray(seed, Class = "BPCellsArray")
    object@delayed <- TRUE
    object
})

###################################################################
.show_internal <- function(object) {
    methods::callNextMethod()
    delayed <- object@delayed
    cat("\n")
    cat(sprintf(
        "`@seed` stored in %s format\n",
        if (delayed) "DelayedArray" else "BPCells"
    ))
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
#' @importMethodsFrom DelayedArray drop
#' @noRd
NULL

### S3/S4 combo for aperm.BPCellsMatrix
# list_methods("DelayedAperm")
aperm.BPCellsMatrix <- call_DelayedArray_method(
    a = , perm = , ... = , type = "S3", Array = "a"
)

#' @importFrom BiocGenerics aperm
methods::setMethod(
    "aperm", "BPCellsMatrix",
    call_DelayedArray_method(a = , perm = , ... = , Array = "a")
)

# S3/S4 combo for as.array.BPCellsMatrix
#' @inheritParams BPCellsSeed-class
#' @exportS3Method base::as.array
#' @rdname BPCellsMatrix-class
as.array.BPCellsMatrix <- function(x, drop = FALSE) {
    assert_bool(drop)
    mat <- as.matrix(x)
    if (drop) drop(mat) else mat
}

#' @return
#'  - `as.array`: A dense matrix or an atomic vector.
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("as.array", "BPCellsMatrix", as.array.BPCellsMatrix)

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
#' @return
#' - `t`: A `BPCellsMatrix` object.
#' @importMethodsFrom BPCells t
#' @export
#' @aliases t
#' @rdname BPCellsMatrix-class
methods::setMethod(
    "t", "BPCellsMatrix",
    set_BPCellsArray_method(
        x = , before = expression(delayed <- x@delayed),
        after = expression(with_delayed(delayed, DelayedArray(object))),
        Arrays = "x"
    )
)

#' @importFrom methods Ops
methods::setMethod(
    "Ops", c("BPCellsArray", "vector"),
    call_DelayedArray_method(e1 = , e2 = , Array = "e1")
)

methods::setMethod(
    "Ops", c("vector", "BPCellsArray"),
    call_DelayedArray_method(e1 = , e2 = , Array = "e2")
)

methods::setMethod(
    "Ops", c("BPCellsArray", "BPCellsArray"),
    call_DelayedArray_method(e1 = , e2 = , Array = "e1")
)
