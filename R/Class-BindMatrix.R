############################################################
# Class `ColBindMatrices`
mould_BPCells("BPCellsDelayedColBind", "ColBindMatrices",
    rename = c(matrix_list = "seeds"),
    contains = "BPCellsDelayedNaryOp"
)

methods::setMethod("to_DelayedArray", "ColBindMatrices", function(object) {
    object <- rename_slot(
        Object = object, matrix_list = "seeds",
        Class = "BPCellsDelayedColBind"
    )
    object@seeds <- lapply(object@seeds, to_DelayedArray)
    object
})

methods::setMethod("to_BPCells", "BPCellsDelayedColBind", function(object) {
    object@seeds <- lapply(object@seeds, to_BPCells)
    rename_slot(
        Object = object,
        seeds = "matrix_list", Class = "ColBindMatrices"
    )
})

summary.BPCellsDelayedColBind <- function(object) {
    sprintf(
        "Concatenate %s of %d matrix objects (threads=%d)",
        if (object@transpose) "rows" else "cols",
        length(object@seeds),
        object@threads
    )
}

methods::setMethod(
    "summary", "BPCellsDelayedColBind",
    summary.BPCellsDelayedColBind
)

############################################################
# Class `RowBindMatrices`
mould_BPCells("BPCellsDelayedRowBind", "RowBindMatrices",
    rename = c(matrix_list = "seeds"),
    contains = "BPCellsDelayedNaryOp"
)

methods::setMethod("to_DelayedArray", "RowBindMatrices", function(object) {
    object <- rename_slot(
        Object = object, matrix_list = "seeds",
        Class = "BPCellsDelayedRowBind"
    )
    object@seeds <- lapply(object@seeds, to_DelayedArray)
    object
})

methods::setMethod("to_BPCells", "BPCellsDelayedRowBind", function(object) {
    object@seeds <- lapply(object@seeds, to_BPCells)
    rename_slot(
        Object = object,
        seeds = "matrix_list", Class = "RowBindMatrices"
    )
})

summary.BPCellsDelayedRowBind <- function(object) {
    sprintf(
        "Concatenate %s of %d matrix objects (threads=%d)",
        if (object@transpose) "cols" else "rows",
        length(object@seeds),
        object@threads
    )
}
methods::setMethod(
    "summary", "BPCellsDelayedRowBind",
    summary.BPCellsDelayedRowBind
)

##############################################################
#' Set matrix op thread count
#'
#' Set number of threads to use for sparse-dense multiply and matrix_stats.
#'
#' @param object A `BPCellsMatrix` object with a seed slot of
#' `BPCellsDelayedColBind`, or `BPCellsDelayedRowBind` object.
#' @param threads Number of threads to use for execution.
#' @param ... Additional arguments to specific methods.
#' @return A `BPCellsMatrix` object.
#' @export
#' @name set_threads
methods::setGeneric("set_threads", function(object, ...) {
    standardGeneric("set_threads")
})

.set_threads_internal <- function(object, threads = 0L) {
    threads <- as.integer(max(0L, threads, na.rm = TRUE))
    object@threads <- threads
    object
}

#' @export
#' @rdname internal-methods
methods::setMethod(
    "set_threads", "BPCellsDelayedColBind",
    .set_threads_internal
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "set_threads", "BPCellsDelayedRowBind",
    .set_threads_internal
)

#' @export
#' @rdname set_threads
methods::setMethod(
    "set_threads", "BPCellsMatrix",
    function(object, threads = 0L) {
        object <- object@seed
        DelayedArray(methods::callGeneric())
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod("set_threads", "ANY", function(object, ...) {
    cli::cli_abort(
        "{.arg object@seed} must be a {.cls BPCellsDelayedColBind} or a {.cls BPCellsDelayedRowBind} object"
    )
})

###################################################################
###########################  Methods  #############################
###################################################################

#' Combine two Objects by Columns or Rows
#'
#' @param x,y A [BPCellsMatrix][BPCellsMatrix-class] or
#' [BPCellsSeed][BPCellsSeed-class] object.
#' @param ...
#'  - `rbind2` and `cbind2`: Not used currently.
#'  - `rbind`, `arbind`, `cbind`, and `acbind`: A list of
#'    [BPCellsMatrix][BPCellsMatrix-class] or [BPCellsSeed][BPCellsSeed-class]
#'    object.
#' @param threads Set the number of threads to use for sparse-dense multiply and
#' [matrix_stats][BPCells::matrix_stats].
#' @seealso
#' [convert_mode]
#' @return
#' If `mode` is specified, the mode of all specified object will be converted.
#' - `cbind2`, `acbind`, `cbind`, `bindCOLS`: A
#'   [BPCellsMatrix][BPCellsMatrix-class] or [BPCellsSeed][BPCellsSeed-class]
#'   object combined by columns.
#' - `rbind2`, `arbind`, `rbind`, `bindROWS`: A
#'   [BPCellsMatrix][BPCellsMatrix-class] or [BPCellsSeed][BPCellsSeed-class]
#'   object combined by rows.
#' @aliases rbind2 cbind2 rbind cbind arbind acbind bindROWS bindCOLS
#' @name BPCells-bind
NULL

#' @importFrom methods rbind2
#' @export
#' @rdname BPCells-bind
methods::setMethod(
    "rbind2", c(x = "BPCellsMatrix", y = "BPCellsMatrix"),
    function(x, y, mode = NULL, ..., threads = 0L) {
        out <- combine_matrices("rbind2",
            mode = mode,
            matrices = list(x, y), ...
        )
        set_threads(out, threads = threads)
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "rbind2", c(x = "ANY", y = "BPCellsMatrix"),
    function(x, y, ...) {
        cli::cli_abort(c(
            "Cannot combine {.cls {obj_type_friendly(x)}} object with a {.cls {obj_type_friendly(y)}}",
            i = "{.arg x} must be a {.cls BPCellsMatrix} object"
        ))
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "rbind2", c(x = "BPCellsMatrix", y = "ANY"),
    function(x, y, ...) {
        cli::cli_abort(c(
            "Cannot combine {.cls {obj_type_friendly(x)}} object with a {.cls {obj_type_friendly(y)}}",
            i = "{.arg y} must be a {.cls BPCellsMatrix} object"
        ))
    }
)

#' @param use.first.dimnames Ignored, always be `TRUE` in BPCells.
#' @param deparse.level Ignored, used by generic methods.
#' @importFrom DelayedArray rbind
#' @export
#' @rdname BPCells-bind
methods::setMethod(
    "rbind", "BPCellsMatrix",
    function(..., mode = NULL, threads = 0L, use.first.dimnames = TRUE, deparse.level = 1L) {
        out <- combine_matrices(
            .fn = "rbind2", mode = mode,
            matrices = pack_BPCellsMatrices(...)
        )
        set_threads(out, threads = threads)
    }
)

#' @importFrom DelayedArray arbind
#' @export
#' @rdname BPCells-bind
methods::setMethod("arbind", "BPCellsMatrix", function(..., mode = NULL, threads = 0L) {
    out <- combine_matrices(
        .fn = "rbind2", mode = mode,
        matrices = pack_BPCellsMatrices(...)
    )
    set_threads(out, threads = threads)
})

#' @param use.names Ignored, always be `TRUE`.
#' @param ignore.mcols Ignored.
#' @param check Ignored.
#' @inheritParams S4Vectors::bindROWS
#' @importFrom S4Vectors bindROWS
#' @export
#' @rdname BPCells-bind
methods::setMethod(
    "bindROWS", "BPCellsMatrix",
    function(x, objects = list(), use.names = TRUE,
             ignore.mcols = TRUE, check = TRUE) {
        assert_(objects, is.list, "a list")
        check_BPCellsMatrices(objects, "objects")
        combine_matrices(
            .fn = "rbind2", mode = NULL,
            matrices = c(list(x), objects)
        )
    }
)

#################### cbind ########################
#' @importFrom methods cbind2
#' @export
#' @rdname BPCells-bind
methods::setMethod(
    "cbind2", c(x = "BPCellsMatrix", y = "BPCellsMatrix"),
    function(x, y, mode = NULL, ..., threads = 0L) {
        out <- combine_matrices("cbind2",
            mode = mode,
            matrices = list(x, y), ...
        )
        set_threads(out, threads = threads)
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "cbind2", c(x = "ANY", y = "BPCellsMatrix"),
    function(x, y, ...) {
        cli::cli_abort(c(
            "Cannot combine {.cls {obj_type_friendly(x)}} object with a {.cls {obj_type_friendly(y)}}",
            i = "{.arg x} must be a {.cls BPCellsMatrix} object"
        ))
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "cbind2", c(x = "BPCellsMatrix", y = "ANY"),
    function(x, y, ...) {
        cli::cli_abort(c(
            "Cannot combine {.cls {obj_type_friendly(x)}} object with a {.cls {obj_type_friendly(y)}}",
            i = "{.arg y} must be a {.cls BPCellsMatrix} object"
        ))
    }
)

#' @importFrom DelayedArray cbind
#' @export
#' @rdname BPCells-bind
methods::setMethod(
    "cbind", "BPCellsMatrix",
    function(..., mode = NULL, threads = 0L, use.first.dimnames = TRUE, deparse.level = 1L) {
        out <- combine_matrices(
            .fn = "cbind2", mode = mode,
            matrices = pack_BPCellsMatrices(...)
        )
        set_threads(out, threads = threads)
    }
)

#' @importFrom DelayedArray acbind
#' @export
#' @rdname BPCells-bind
methods::setMethod("acbind", "BPCellsMatrix", function(..., mode = NULL, threads = 0L, use.first.dimnames = TRUE) {
    out <- combine_matrices(
        .fn = "cbind2", mode = mode,
        matrices = pack_BPCellsMatrices(...)
    )
    set_threads(out, threads = threads)
})

#' @importFrom S4Vectors bindCOLS
#' @export
#' @rdname BPCells-bind
methods::setMethod(
    "bindCOLS", "BPCellsMatrix",
    function(x, objects = list(), use.names = TRUE,
             ignore.mcols = TRUE, check = TRUE) {
        assert_(objects, is.list, "a list")
        check_BPCellsMatrices(objects, "objects")
        combine_matrices(
            .fn = "cbind2", mode = NULL,
            matrices = c(list(x), objects)
        )
    }
)

#################################################################
combine_matrices <- function(.fn, mode, matrices, ...) {
    fn <- methods::getMethod(.fn,
        c("IterableMatrix", "IterableMatrix"),
        where = "BPCells"
    )
    seeds <- lapply(matrices, function(matrix) to_BPCells(matrix@seed))
    if (is.null(mode)) {
        mode <- compatible_storage_mode(seeds)
    } else {
        mode <- match.arg(mode, BPCells_MODE)
    }
    seeds <- lapply(seeds, function(seed, mode) {
        if (storage_mode(seed) != mode) {
            cli::cli_inform("Converting into {mode} data type")
            BPCells::convert_matrix_type(seed, type = mode)
        } else {
            seed
        }
    }, mode = mode)
    seed <- Reduce(function(x, y) fn(x = x, y = y, ...), seeds)
    DelayedArray(to_DelayedArray(seed))
}

pack_BPCellsMatrices <- function(..., call = rlang::caller_env()) {
    objects <- list(...)
    check_BPCellsMatrices(objects, arg = "...", call = call)
    objects
}

check_BPCellsMatrices <- function(list, arg = rlang::caller_arg(list), call = rlang::caller_env()) {
    BPCellsMatrices <- vapply(list,
        methods::is, logical(1L),
        class2 = "BPCellsMatrix",
        USE.NAMES = FALSE
    )
    if (!all(BPCellsMatrices)) {
        cli::cli_abort(
            c(
                "all input must be a {.cls BPCellsMatrix} object",
                i = "Please check the input {.arg {arg}} in {.val {which(!BPCellsMatrices)}}"
            ),
            call = call
        )
    }
}
