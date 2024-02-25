#' BPCellsArray seed management
#'
#' @description
#' `BPCellsArray` offers two methods for using BPCells Matrix as the
#' DelayedArray seed:
#' - `delayed=FALSE`: use the `IterableMatrix` of BPCells as the seed directly.
#' - `delayed=TRUE`: convert `IterableMatrix` into a parallel
#'   [DelayedOp][DelayedArray::DelayedOp-class] object (`BPCellsDelayedOp`).
#'
#' Both methods are generally effective for most operations. However, choosing
#' `delayed=TRUE` ensures better compatibility with `DelayedArray`. It should be
#' noted that [showtree][DelayedArray::showtree] and
#' [seedApply][DelayedArray::seedApply] from DelayedArray do not function well
#' when `delayed=FALSE`. Nonetheless, BPCellsArray provides its own [seedApply]
#' and [showtree] functions for both methods. Overall, executing operations with
#' `delayed=FALSE` is expected to be faster due to the absence of the need to
#' convert between `IterableMatrix` and `BPCellsDelayedOp` objects.
#' `delayed=FALSE` is also the default method.
#'
#' @param ... Additional argumentds passed into specific methods.
#' @return
#' - `missing`: Get current default `delayed` value.
#' - `logical`: Change the default `delayed` value and return original default
#'   `delayed` value invisiblely.
#' - `BPCellsMatrix`: Change the `delayed` value of a [BPCellsMatrix] object.
#' @export
methods::setGeneric("set_delayed", function(x, ...) {
    standardGeneric("set_delayed")
})

#' @param x See return section
#' @export
#' @rdname set_delayed
methods::setMethod("set_delayed", "missing", function(x) {
    GlobalOptions$DelayedBPCells
})

#' @export
#' @rdname set_delayed
methods::setMethod("set_delayed", "logical", function(x) {
    msg <- "{.arg x} must be a single bool value"
    if (length(x) != 1L) {
        cli::cli_abort(c(msg, i = "You have provided a length {length(x)}"))
    } else if (is.na(x)) {
        cli::cli_abort(c(msg, i = "{.code NA} is not allowed"))
    }
    old <- GlobalOptions$DelayedBPCells
    GlobalOptions$DelayedBPCells <- x
    invisible(old)
})

#' @include Class-BPCellsMatrix.R
#' @param delayed A bool value.
#' @export
#' @rdname set_delayed
methods::setMethod("set_delayed", "BPCellsMatrix", function(x, delayed = NULL) {
    delayed <- delayed %||% GlobalOptions$DelayedBPCells
    if (x@delayed == delayed) {
        if (delayed) {
            msg <- "{.arg x@seed} is already in {.pkg DelayedArray} format"
        } else {
            msg <- "{.arg x@seed} is already in {.pkg BPCells} format"
        }
        cli::cli_inform(c_msg(msg, "nothing to do", sep = ", "))
        return(x)
    }
    with_delayed(delayed, DelayedArray(x@seed))
})

with_delayed <- function(delayed, code, envir = parent.frame()) {
    old <- set_delayed(delayed)
    on.exit(set_delayed(old))
    eval(substitute(code), envir = envir)
}
