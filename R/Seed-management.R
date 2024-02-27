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
#' `seedform=DelayedArray` ensures better compatibility with DelayedArray
#' package. It should be noted that [showtree][DelayedArray::showtree] and
#' [seedApply][DelayedArray::seedApply] from DelayedArray do not function well
#' when `seedform=BPCells`. Nonetheless, BPCellsArray provides its own
#' [seedApply] and [showtree] functions for both methods. Overall, executing
#' operations with `seedform=BPCells` is expected to be faster due to the
#' absence of the need to convert between `IterableMatrix` and
#' `BPCellsDelayedOp` objects. `seedform=BPCells` is also the default method.
#'
#' @param ... Additional argumentds passed into specific methods.
#' @return
#' - `missing`: Get current default `seedform` value.
#' - `character`: Change the default `seedform` value and return the original
#'    default `seedform` value invisiblely.
#' - `BPCellsMatrix`:
#'    * For `seedform` method: Get the seedform of [BPCellsMatrix] object.
#'    * For `seedform<-` method: Change the `seedform` value of a
#'      [BPCellsMatrix] object.
#' @export
methods::setGeneric("seedform", function(x) {
    standardGeneric("seedform")
})

#' @param x See `Value` section
#' @export
#' @rdname seedform
methods::setMethod("seedform", "missing", function(x) {
    get_seedform()
})

#' @export
#' @rdname seedform
methods::setMethod("seedform", "character", function(x) {
    x <- match_seedform(x)
    old <- get_seedform()
    set_seedform(x)
    invisible(old)
})

with_seedform <- function(seedform, code, envir = parent.frame()) {
    old <- get_seedform()
    on.exit(set_seedform(old))
    eval(substitute(code), envir = envir)
}

get_seedform <- function() GlobalOptions$SeedForm

set_seedform <- function(seedform) GlobalOptions$SeedForm <- seedform

match_seedform <- function(seedform, default = get_seedform()) {
    if (is.null(seedform)) default else match.arg(seedform, AllowedSeedForm)
}

# Global options control whether use `to_DelayedArray` to convert
# BPCells matrix into a `DelayedOp` object
GlobalOptions <- new.env(parent = emptyenv())
AllowedSeedForm <- c("BPCells", "DelayedArray")
set_seedform("BPCells")

# helper function used by other funcionts ---------------------------
extract_seed_and_seedform <- function(x, seedform) {
    if (is_BPCellsArray(x)) {
        seed <- to_BPCells(x@seed) # nolint
        seedform <- match_seedform(seedform, x@SeedForm) # nolint
    } else {
        seed <- BPCellsSeed(x)
        seedform <- match_seedform(seedform)
    }
    list(seed = seed, seedform = seedform)
}

.validate_seedform <- function(seedform, arg = rlang::caller_arg(seedform)) {
    msg <- c_msg(
        "{.arg {arg}} must be a single string",
        "of {.val BPCells} or {.val DelayedArray}"
    )
    if (length(seedform) != 1L) {
        cli::cli_abort(
            c(msg, i = "You have provided a length {length(seedform)}")
        )
    } else if (is.na(seedform) || !any(seedform == AllowedSeedForm)) {
        cli::cli_abort(c(msg, i = "{.val {seedform}} is not allowed"))
    }
    TRUE
}
