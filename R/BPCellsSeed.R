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
    # catch the argumeent passed into `BPCellsSeed` function
    x <- coerce_into_dgCMatrix(x, arg = substitute(x, env = sys.frame(-2L)))
    methods::callGeneric()
})

coerce_into_dgCMatrix <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
    rlang::try_fetch(
        methods::as(x, "dgCMatrix"),
        error = function(cnd) {
            cli::cli_abort(
                "{.arg {arg}} must be a matrix-like object which can be coerced into {.cls dgCMatrix}",
                call = call
            )
        }
    )
}
