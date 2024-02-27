#' Build seed object for `BPCellsMatrix`
#'
#' @param x A `IterableMatrix` object from `BPCells` or a matrix-like object
#' which can be coerced into dgCMatrix object.
#' @return A `IterableMatrix` object.
#' @name BPCellsSeed
#' @seealso [BPCellsSeed][BPCellsSeed-class] contract
NULL

############################################################
#' @export
#' @rdname BPCellsSeed
methods::setGeneric("BPCellsSeed", function(x) {
    standardGeneric("BPCellsSeed")
})

############################################################
#' @include Class-BPCellsSeed.R
#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "IterableMatrix", function(x) x)

#' @include Class-Delayed.R
#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "BPCellsDelayedOp", function(x) {
    to_BPCells(x)
})

##############################################################
#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "matrix", function(x) {
    mode <- storage_mode(x)
    x <- methods::as(x, "dgCMatrix")
    seed <- methods::callGeneric()
    BPCells::convert_matrix_type(matrix = seed, type = mode)
})

############################################################
# Iterable_dgCMatrix_wrapper
summary.Iterable_dgCMatrix_wrapper <- function(object) {
    "Load dgCMatrix from memory"
}

methods::setMethod(
    "summary", "Iterable_dgCMatrix_wrapper",
    summary.Iterable_dgCMatrix_wrapper
)

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "dgCMatrix", function(x) {
    methods::as(x, "IterableMatrix")
})

#############################################################
#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "ANY", function(x) {
    # prepare for message usage
    # catch the argumeent passed into `BPCellsSeed` function
    arg <- substitute(x, env = sys.frame(-2L)) # nolint
    # Get current environemnt
    current_env <- environment()
    x <- tryCatch(
        methods::as(x, "dgCMatrix"),
        error = function(cnd) {
            cli::cli_abort(
                "{.arg {arg}} must be a matrix-like object which can be coerced into {.cls dgCMatrix}",
                call = current_env
            )
        }
    )
    methods::callGeneric()
})
