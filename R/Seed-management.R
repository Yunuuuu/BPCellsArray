# Global options control whether use `to_DelayedArray` to convert
# BPCells matrix into a `DelayedOp` object
GlobalOptions <- new.env(parent = emptyenv())
GlobalOptions$SeedForm <- "BPCells"

#' BPCellsArray seed management
#'
#' @description
#' `BPCellsArray` offers two seed form for using BPCells Matrix as the
#' DelayedArray seed:
#' - `BPCells`: use the `IterableMatrix` of BPCells as the seed directly.
#' - `DelayedArray`: convert `IterableMatrix` into a parallel
#'   [DelayedOp][DelayedArray::DelayedOp-class] object (`BPCellsDelayedOp`).
#'
#' Both methods are generally effective for most operations. However, choosing
#' `seed_form=DelayedArray` ensures better compatibility with DelayedArray
#' package. It should be noted that [showtree][DelayedArray::showtree] and
#' [seedApply][DelayedArray::seedApply] from DelayedArray do not function well
#' when `seed_form=BPCells`. Nonetheless, BPCellsArray provides its own
#' [seedApply] and [showtree] functions for both methods. Overall, executing
#' operations with `seed_form=BPCells` is expected to be faster due to the
#' absence of the need to convert between `IterableMatrix` and
#' `BPCellsDelayedOp` objects. `seed_form=BPCells` is also the default method.
#'
#' @param ... Additional argumentds passed into specific methods.
#' @return
#' - `missing`: Get current default `seed_form` value.
#' - `character`: Change the default `seed_form` value and return the original
#'   default `seed_form` value invisiblely.
#' - `BPCellsMatrix`: Change the `seed_form` value of a [BPCellsMatrix] object.
#' @export
methods::setGeneric("set_seed_form", function(x, ...) {
    standardGeneric("set_seed_form")
})

#' @param x See `Value` section
#' @export
#' @rdname set_seed_form
methods::setMethod("set_seed_form", "missing", function(x) {
    GlobalOptions$SeedForm
})

#' @export
#' @rdname set_seed_form
methods::setMethod("set_seed_form", "character", function(x) {
    x <- match_seed_form(x)
    old <- GlobalOptions$SeedForm
    GlobalOptions$SeedForm <- x
    invisible(old)
})

with_seed_form <- function(seed_form, code, envir = parent.frame()) {
    old <- set_seed_form(seed_form)
    on.exit(set_seed_form(old))
    eval(substitute(code), envir = envir)
}

match_seed_form <- function(seed_form) {
    if (is.null(seed_form)) {
        GlobalOptions$SeedForm
    } else {
        match.arg(seed_form, c("BPCells", "DelayedArray"))
    }
}

.validate_seed_form <- function(seed_form, arg = rlang::caller_arg(seed_form)) {
    msg <- "{.arg {arg}} must be a single string of {.val BPCells} or {.val DelayedArray}"
    if (length(seed_form) != 1L) {
        cli::cli_abort(
            c(msg, i = "You have provided a length {length(seed_form)}")
        )
    } else if (!any(seed_form == c("BPCells", "DelayedArray"))) {
        cli::cli_abort(c(msg, i = "{.val {seed_form}} is not allowed"))
    }
    return(TRUE)
}
