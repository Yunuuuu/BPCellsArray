# No need to mould `RowBindMatrices` Class
mould_BPCells("BPCellsDelayedAbind", "ColBindMatrices",
    remove = "matrix_list",
    # DelayedAbind: `along` slot
    # BPCellsDelayedNaryOp: `seeds` slot
    contains = c("DelayedAbind", "BPCellsDelayedNaryOp")
)

### list_methods("DelayedAbind")
### Seed contract
### here: we override the `DelayedAbind` methods
methods::setMethod(
    "dim", "BPCellsDelayedAbind",
    delayedop_call_BPCells_method(x = , Array = "x")
)

methods::setMethod(
    "dimnames", "BPCellsDelayedAbind",
    delayedop_call_BPCells_method(x = , Array = "x")
)

methods::setMethod("is_sparse", "BPCellsDelayedAbind", function(x) TRUE)
methods::setMethod(
    "extract_array", "BPCellsDelayedAbind",
    delayedop_call_BPCells_method(x = , index = , Array = "x")
)

methods::setMethod(
    "OLD_extract_sparse_array", "BPCellsDelayedAbind",
    delayedop_call_BPCells_method(x = , index = , Array = "x")
)

############################################################
to_DelayedAbind <- function(object, along) {
    object <- migrate_slots(
        Object = object,
        rename = c(matrix_list = "seeds"),
        Class = "BPCellsDelayedAbind"
    )
    object@along <- along
    object@seeds <- lapply(object@seeds, to_DelayedArray)
    object
}

# Class `ColBindMatrices`
methods::setMethod("to_DelayedArray", "ColBindMatrices", function(object) {
    to_DelayedAbind(object, 2L)
})

# Class `RowBindMatrices`
methods::setMethod("to_DelayedArray", "RowBindMatrices", function(object) {
    to_DelayedAbind(object, 1L)
})

methods::setMethod("to_BPCells", "BPCellsDelayedAbind", function(object) {
    object@seeds <- lapply(object@seeds, to_BPCells)
    if (object@along == 1L) {
        migrate_slots(
            Object = object,
            remove = "along",
            rename = c(seeds = "matrix_list"),
            Class = "RowBindMatrices"
        )
    } else {
        migrate_slots(
            Object = object,
            remove = "along",
            rename = c(seeds = "matrix_list"),
            Class = "ColBindMatrices"
        )
    }
})

summary.BPCellsDelayedAbind <- function(object) {
    if (object@along == 1L) {
        along <- if (object@transpose) "cols" else "rows"
    } else {
        along <- if (object@transpose) "rows" else "cols"
    }
    sprintf(
        "Concatenate %s of %d matrix objects (threads=%d)",
        along,
        length(object@seeds),
        object@threads
    )
}

methods::setMethod(
    "summary", "BPCellsDelayedAbind",
    summary.BPCellsDelayedAbind
)

summary.RowBindMatrices <- function(object) {
    sprintf(
        "Concatenate %s of %d matrix objects (threads=%d)",
        if (object@transpose) "cols" else "rows",
        length(object@matrix_list),
        object@threads
    )
}
methods::setMethod("summary", "RowBindMatrices", summary.RowBindMatrices)
summary.ColBindMatrices <- function(object) {
    sprintf(
        "Concatenate %s of %d matrix objects (threads=%d)",
        if (object@transpose) "rows" else "cols",
        length(object@matrix_list),
        object@threads
    )
}
methods::setMethod("summary", "ColBindMatrices", summary.ColBindMatrices)

##############################################################
#' Set matrix op thread count
#'
#' Set number of threads to use for sparse-dense multiply and matrix_stats.
#'
#' @param object A [BPCellsMatrix] object with a seed slot of
#' `BPCellsDelayedAbind` object, usually derived from [bind][BPCells-bind]
#' operators.
#' @param threads Set the number of threads to use for sparse-dense multiply and
#' [matrix_stats][BPCells::matrix_stats].
#' @param ... Additional arguments to specific methods.
#' @return A [BPCellsMatrix] object.
#' @export
#' @name set_threads
methods::setGeneric("set_threads", function(object, ...) {
    standardGeneric("set_threads")
})

#' @export
#' @rdname set_threads
methods::setMethod(
    "set_threads", "BPCellsMatrix",
    function(object, threads = 0L) {
        delayed <- object@delayed
        object <- object@seed
        with_delayed(delayed, DelayedArray(methods::callGeneric()))
    }
)

.set_threads_internal <- function(object, threads = 0L) {
    threads <- as.integer(max(0L, threads, na.rm = TRUE))
    object@threads <- threads
    object
}

#' @inheritParams set_threads
#' @export
#' @rdname internal-methods
methods::setMethod("set_threads", "BPCellsDelayedAbind", .set_threads_internal)

#' @export
#' @rdname internal-methods
methods::setMethod("set_threads", "ColBindMatrices", .set_threads_internal)

#' @export
#' @rdname internal-methods
methods::setMethod("set_threads", "RowBindMatrices", .set_threads_internal)

#' @export
#' @rdname internal-methods
methods::setMethod("set_threads", "ANY", function(object, ...) {
    cli::cli_abort(
        "{.arg object@seed} must be a {.cls BPCellsDelayedAbind}, {.cls ColBindMatrices}, or {.cls ColBindMatrices} object"
    )
})

###################################################################
###########################  Methods  #############################
###################################################################

#' Combine two Objects by Columns or Rows
#'
#' @param x,y A [BPCellsMatrix][BPCellsMatrix-class] or
#' [BPCellsSeed][BPCellsSeed-class] object.
#' @inheritParams convert_mode
#' @param ...
#'  - `rbind2` and `cbind2`: Not used currently.
#'  - `rbind`, `arbind`, `cbind`, and `acbind`: A list of
#'    [BPCellsMatrix][BPCellsMatrix-class] object.
#' @inheritParams set_threads
#' @seealso
#' [convert_mode]
#' @return
#' If `mode` is specified, the mode of all specified object will be converted.
#' - `cbind2`, `acbind`, `cbind`, `bindCOLS`: A
#'   [BPCellsMatrix][BPCellsMatrix-class] object combined by columns.
#' - `rbind2`, `arbind`, `rbind`, `bindROWS`: A
#'   [BPCellsMatrix][BPCellsMatrix-class] object combined by rows.
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
            "Cannot combine {.cls {obj_s4_friendly(x)}} object with a {.cls {obj_s4_friendly(y)}}",
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
            "Cannot combine {.cls {obj_s4_friendly(x)}} object with a {.cls {obj_s4_friendly(y)}}",
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
            "Cannot combine {.cls {obj_s4_friendly(x)}} object with a {.cls {obj_s4_friendly(y)}}",
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
            "Cannot combine {.cls {obj_s4_friendly(x)}} object with a {.cls {obj_s4_friendly(y)}}",
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
compatible_storage_mode <- function(list) {
    actual_modes <- vapply(
        list, storage_mode, character(1L),
        USE.NAMES = FALSE
    )
    BPCells_MODE[max(match(actual_modes, BPCells_MODE))]
}

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
    with_delayed(matrices[[1L]]@delayed, DelayedArray(seed))
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
