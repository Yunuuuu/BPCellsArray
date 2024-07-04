#' Transform into `IterableMatrix`
#'
#' @param x A `IterableMatrix` object from `BPCells` or a `r rd_seed()`.
#' @return A `IterableMatrix` object.
#' @name BPCellsSeed
#' @seealso
#' - [BPCellsSeed]
#' - [BPCellsDir-IO]
#' - [BPCellsHDF5-IO]
#' - [BPCellsMem-IO]
#' - [BPCells10xHDF5-IO]
NULL

############################################################
#' @export
#' @rdname BPCellsSeed
methods::setGeneric("BPCellsSeed", function(x) {
    standardGeneric("BPCellsSeed")
})

############################################################
#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "IterableMatrix", function(x) x)

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
#' @exportS3Method base::summary
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
    x <- coerce_into_dgCMatrix(x)
    methods::callGeneric()
})

# must be used in a Generic method
coerce_into_dgCMatrix <- function(x) {
    # n = -1: method function environment
    # n = -2: Generic function environment
    # n = -3: the caller environment of Generic function
    caller_env <- sys.frame(-3L)
    pkg_env <- topenv(caller_env)
    if (isNamespace(pkg_env) && getNamespaceName(pkg_env) == pkg_nm()) {
        # if called from the package internal, we catch the argument
        # passed into BPCellsSeed
        call <- caller_env
        arg <- substitute(x, sys.frame(-2L)) # nolint
    } else {
        # if called from global environment or other package
        # catch the argument of method argument directly
        call <- sys.frame(-1L)
        arg <- substitute(x)
    }
    tryCatch(
        methods::as(x, "dgCMatrix"),
        error = function(cnd) {
            cli::cli_abort(
                paste(
                    "{.arg {arg}} must be a matrix-like object",
                    "which can be coerced into {.cls dgCMatrix}"
                ),
                call = call
            )
        }
    )
}

# helper function used to extract the `IterableMatrix` from the user input,
# although user shouldn't provide `BPCellsDelayedOp` directly, but we also deal
# with it
extract_IterableMatrix <- function(x) {
    if (is_BPCellsArray(x)) {
        to_BPCells(x@seed)
    } else if (methods::is(x, "BPCellsDelayedOp")) {
        to_BPCells(x)
    } else {
        BPCellsSeed(x)
    }
}
