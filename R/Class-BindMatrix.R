############################################################
# Class for `ColBindMatrices` and `RowBindMatrices`

methods::setClass("BPCellsBindMatrixSeed",
    contains = c("BPCellsNaryOpsSeed", "VIRTUAL")
)

methods::setValidity("BPCellsBindMatrixSeed", function(object) {
    BPCellsSeeds <- vapply(object@matrix_list,
        methods::is, logical(1L),
        class2 = "BPCellsSeed",
        USE.NAMES = FALSE
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
    contains = c("BPCellsBindMatrixSeed", BPCells_class("ColBindMatrices"))
)

methods::setClass("BPCellsRowBindMatrixSeed",
    contains = c("BPCellsBindMatrixSeed", BPCells_class("RowBindMatrices"))
)

####################################################################
methods::setMethod("summary", "BPCellsColBindMatrixSeed", function(object) {
    sprintf(
        "Concatenate %s of %d matrix objects (threads=%d)",
        if (object@transpose) "rows" else "cols",
        length(object@matrix_list),
        object@threads
    )
})
methods::setMethod("summary", "BPCellsRowBindMatrixSeed", function(object) {
    sprintf(
        "Concatenate %s of %d matrix objects (threads=%d)",
        if (object@transpose) "cols" else "rows",
        length(object@matrix_list),
        object@threads
    )
})

####################################################################
#' @return
#' - `BPCellsColBindMatrixSeed`: A `BPCellsColBindMatrixSeed` object.
#' - `BPCellsRowBindMatrixSeed`: A `BPCellsRowBindMatrixSeed` object.
#' @noRd
BPCellsBindMatrixSeed <- function(x, class) {
    x@matrix_list <- lapply(x@matrix_list, BPCellsSeed)
    methods::as(x, Class = class)
}

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "ColBindMatrices", function(x) {
    BPCellsBindMatrixSeed(x = x, class = "BPCellsColBindMatrixSeed")
})

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "RowBindMatrices", function(x) {
    BPCellsBindMatrixSeed(x = x, class = "BPCellsRowBindMatrixSeed")
})

methods::setMethod("entity", "BPCellsBindMatrixSeed", function(x) {
    x@matrix_list
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
    standardGeneric("set_threads")
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
        DelayedArray(rbind2(
            x = x@seed, y = y@seed, mode = mode,
            ..., threads = threads
        ))
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
        DelayedArray(cbind2(
            x = x@seed, y = y@seed, mode = mode, ..., threads = threads
        ))
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

combine_matrices <- function(.fn, mode, matrices, ...) {
    seeds <- lapply(matrices, methods::slot, name = "seed")
    DelayedArray(combine_seeds(.fn = .fn, mode = mode, seeds = seeds, ...))
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


#################### BPCellsSeed methods ########################
#' @importFrom methods rbind2
#' @inheritParams convert_mode
#' @export
#' @rdname BPCells-bind
methods::setMethod(
    "rbind2", c(x = "BPCellsSeed", y = "BPCellsSeed"),
    function(x, y, mode = NULL, ..., threads = 0L) {
        out <- combine_seeds("rbind2", mode = mode, seeds = list(x, y), ...)
        set_threads(out, threads = threads)
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "rbind2", c(x = "ANY", y = "BPCellsSeed"),
    function(x, y, ...) {
        cli::cli_abort(c(
            "Cannot combine {.cls {obj_type_friendly(x)}} object with a {.cls {obj_type_friendly(y)}}",
            i = "{.arg x} must be a {.cls BPCellsSeed} object"
        ))
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "rbind2", c(x = "BPCellsSeed", y = "ANY"),
    function(x, y, ...) {
        cli::cli_abort(c(
            "Cannot combine {.cls {obj_type_friendly(x)}} object with a {.cls {obj_type_friendly(y)}}",
            i = "{.arg y} must be a {.cls BPCellsSeed} object"
        ))
    }
)

#' @inheritParams BPCells-bind
#' @importFrom DelayedArray rbind
#' @export
#' @rdname BPCells-bind
methods::setMethod(
    "rbind", "BPCellsSeed",
    function(..., mode = NULL, threads = 0L,
             use.first.dimnames = TRUE, deparse.level = 1L) {
        out <- combine_seeds(
            .fn = "rbind2", mode = mode,
            seeds = pack_BPCellsSeeds(...)
        )
        set_threads(out, threads = threads)
    }
)

#' @importFrom DelayedArray arbind
#' @export
#' @rdname BPCells-bind
methods::setMethod(
    "arbind", "BPCellsSeed",
    function(..., mode = NULL, threads = 0L, use.first.dimnames = TRUE) {
        out <- combine_seeds(
            .fn = "rbind2", mode = mode,
            seeds = pack_BPCellsSeeds(...)
        )
        set_threads(out, threads = threads)
    }
)

#' @importFrom S4Vectors bindROWS
#' @export
#' @rdname BPCells-bind
methods::setMethod(
    "bindROWS", "BPCellsSeed",
    function(x, objects = list(), use.names = TRUE,
             ignore.mcols = TRUE, check = TRUE) {
        assert_(objects, is.list, "a list")
        check_BPCellsSeeds(objects)
        combine_seeds(
            .fn = "rbind2", mode = NULL,
            seeds = c(list(x), objects)
        )
    }
)

#################### cbind ########################
#' @importFrom methods cbind2
#' @export
#' @rdname BPCells-bind
methods::setMethod(
    "cbind2", c(x = "BPCellsSeed", y = "BPCellsSeed"),
    function(x, y, mode = NULL, ..., threads = 0L) {
        out <- combine_seeds("cbind2", mode = mode, seeds = list(x, y), ...)
        set_threads(out, threads = threads)
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "cbind2", c(x = "ANY", y = "BPCellsSeed"),
    function(x, y, ...) {
        cli::cli_abort(c(
            "Cannot combine {.cls {obj_type_friendly(x)}} object with a {.cls {obj_type_friendly(y)}}",
            i = "{.arg x} must be a {.cls BPCellsSeed} object"
        ))
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "cbind2", c(x = "BPCellsSeed", y = "ANY"),
    function(x, y, ...) {
        cli::cli_abort(c(
            "Cannot combine {.cls {obj_type_friendly(x)}} object with a {.cls {obj_type_friendly(y)}}",
            i = "{.arg y} must be a {.cls BPCellsSeed} object"
        ))
    }
)

#' @importFrom DelayedArray cbind
#' @export
#' @rdname BPCells-bind
methods::setMethod(
    "cbind", "BPCellsSeed",
    function(..., mode = NULL, threads = 0L, use.first.dimnames = TRUE, deparse.level = 1L) {
        out <- combine_seeds(
            .fn = "cbind2", mode = mode,
            seeds = pack_BPCellsSeeds(...)
        )
        set_threads(out, threads = threads)
    }
)

#' @importFrom DelayedArray acbind
#' @export
#' @rdname BPCells-bind
methods::setMethod("acbind", "BPCellsSeed", function(..., mode = NULL, threads = 0L, use.first.dimnames = TRUE) {
    out <- combine_seeds(
        .fn = "cbind2", mode = mode,
        seeds = pack_BPCellsSeeds(...)
    )
    set_threads(out, threads = threads)
})

#' @importFrom S4Vectors bindCOLS
#' @export
#' @rdname BPCells-bind
methods::setMethod(
    "bindCOLS", "BPCellsSeed",
    function(x, objects = list(), use.names = TRUE,
             ignore.mcols = TRUE, check = TRUE) {
        assert_(objects, is.list, "a list")
        check_BPCellsSeeds(objects)
        combine_seeds(
            .fn = "cbind2", mode = NULL,
            seeds = c(list(x), objects)
        )
    }
)

#################################################################
combine_seeds <- function(.fn, mode, seeds, ...) {
    fn <- methods::getMethod(.fn,
        c("IterableMatrix", "IterableMatrix"),
        where = "BPCells"
    )
    if (is.null(mode)) {
        mode <- compatible_storage_mode(seeds)
    } else {
        mode <- match.arg(mode, BPCells_MODE)
    }
    seeds <- lapply(seeds, function(seed, mode) {
        if (storage_mode(seed) != mode) {
            cli::cli_inform("Converting into {mode} data type")
            BPCellsSeed(BPCells::convert_matrix_type(seed, type = mode))
        } else {
            seed
        }
    }, mode = mode)
    out <- Reduce(function(x, y) fn(x = x, y = y, ...), seeds)
    BPCellsSeed(out)
}

pack_BPCellsSeeds <- function(..., call = rlang::caller_env()) {
    objects <- list(...)
    check_BPCellsSeeds(objects, arg = "...", call = call)
    objects
}

check_BPCellsSeeds <- function(list, arg = rlang::caller_arg(list), call = rlang::caller_env()) {
    BPCellsSeeds <- vapply(list,
        methods::is, logical(1L),
        class2 = "BPCellsSeed",
        USE.NAMES = FALSE
    )
    if (!all(BPCellsSeeds)) {
        cli::cli_abort(
            c(
                "all input must be a {.cls BPCellsSeed} object",
                i = "Please check the input {.arg {arg}} in item{?s} of {.val {which(!BPCellsSeeds)}}"
            ),
            call = call
        )
    }
}
