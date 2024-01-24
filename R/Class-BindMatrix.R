methods::setClass("BPCellsBindMatrixSeed",
    contains = c("BPCellsNaryOpsSeed", "VIRTUAL")
)

methods::setValidity("BPCellsBindMatrixSeed", function(object) {
    BPCellsSeeds <- vapply(object@matrix_list,
        methods::is, logical(1L),
        class = "BPCellsSeed"
    )
    if (!all(BPCellsSeeds)) {
        cli::cli_abort(c(
            "all `@matrix_list` must be a {.cls BPCellsSeed} object",
            i = "Please check the `@matrix_list` in: {.val {which(!BPCellsMatrices)}}"
        ))
    }
    return(TRUE)
})

methods::setClass("BPCellsColBindMatrixSeed",
    contains = c("BPCellsBindMatrixSeed", get_class("ColBindMatrices"))
)

methods::setClass("BPCellsRowBindMatrixSeed",
    contains = c("BPCellsBindMatrixSeed", get_class("RowBindMatrices"))
)

#' @return
#' - `BPCellsColBindMatrixSeed`: A `BPCellsColBindMatrixSeed` object.
#' - `BPCellsRowBindMatrixSeed`: A `BPCellsRowBindMatrixSeed` object.
#' @noRd
methods::setGeneric("BPCellsBindMatrixSeed", function(x, ...) {
    x@matrix_list <- lapply(x@matrix_list, BPCellsSeed)
    class <- standardGeneric("BPCellsBindMatrixSeed")
    methods::as(x, Class = class)
})

methods::setMethod(
    "BPCellsBindMatrixSeed", "ColBindMatrices",
    function(x) "BPCellsColBindMatrixSeed"
)

methods::setMethod(
    "BPCellsBindMatrixSeed", "RowBindMatrices",
    function(x) "BPCellsRowBindMatrixSeed"
)

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "ColBindMatrices", function(x) {
    BPCellsBindMatrixSeed(x = x)
})

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "RowBindMatrices", function(x) {
    BPCellsBindMatrixSeed(x = x)
})

##############################################################
#' Set matrix op thread count
#'
#' Set number of threads to use for sparse-dense multiply and matrix_stats.
#'
#'
#' @param object A `BPCellsRowBindMatrixSeed`, `BPCellsColBindMatrixSeed`,
#' `BPCellsColBindMatrixMatrix` or `BPCellsRowBindMatrixMatrix` object.
#' @param threads Number of threads to use for execution.
#' @param ... Additional arguments to specific methods.
#' @return A `BPCellsRowBindMatrixSeed` or `BPCellsRowBindMatrixMatrix` object.
#' @export
#' @name set_threads
methods::setGeneric("set_threads", function(object, ...) {
    makeStandardGeneric("set_threads")
})

#' @export
#' @rdname set_threads
methods::setMethod(
    "set_threads", "BPCellsBindMatrixSeed",
    function(object, threads = 0L) {
        threads <- as.integer(max(0L, threads, na.rm = TRUE))
        object@threads <- threads
        object
    }
)

#' @export
#' @rdname set_threads
methods::setMethod(
    "set_threads", "BPCellsMatrix",
    function(object, threads = 0L) {
        assert_s4_class(object@seed, "BPCellsBindMatrixSeed")
        object@seed <- set_threads(object@seed, threads = threads)
        object
    }
)

#' @export
#' @rdname set_threads
methods::setMethod("set_threads", "ANY", function(object, ...) {
    cli::cli_abort(
        "{.arg object} must be a {.cls BPCellsBindMatrixSeed} or a {.cls BPCellsMatrix} with a {.cls BPCellsBindMatrixSeed} seed slot"
    )
})
