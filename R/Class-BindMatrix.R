############################################################
# `ColBindMatrices` and `RowBindMatrices`

methods::setClass("BPCellsBindMatrixSeed",
    contains = c("BPCellsNaryOpsSeed", "VIRTUAL")
)

methods::setValidity("BPCellsBindMatrixSeed", function(object) {
    BPCellsSeeds <- vapply(object@matrix_list,
        methods::is, logical(1L),
        class2 = "BPCellsSeed"
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

###################################################################
###########################  Methods  #############################
###################################################################

#' Combine two Objects by Columns or Rows
#'
#' @param x,y A [BPCellsMatrix][BPCellsMatrix-class] object.
#' @param ...
#'  - `rbind2` and `cbind2`: Not used currently.
#'  - `rbind`, `arbind`, `cbind`, and `acbind`: A list of
#'    [BPCellsMatrix][BPCellsMatrix-class] objects.
#' @param threads Set number of threads to use for sparse-dense multiply and
#' [matrix_stats][BPCells::matrix_stats].
#' @return
#' - `cbind2`, `acbind`, `cbind`, `bindCOLS`: A
#'   [BPCellsMatrix][BPCellsMatrix-class] object.
#' - `rbind2`, `arbind`, `rbind`, `bindROWS`: A
#'   [BPCellsMatrix][BPCellsMatrix-class] object.
#' @aliases rbind2 cbind2 rbind cbind arbind acbind bindROWS bindCOLS
#' @name BPCells-bind
NULL

#' @importFrom methods rbind2
#' @export
#' @rdname BPCells-bind
methods::setMethod(
    "rbind2", c(x = "BPCellsMatrix", y = "BPCellsMatrix"),
    function(x, y, ..., threads = 0L) {
        DelayedArray(rbind2(x = x@seed, y = y@seed, ..., threads = threads))
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
    function(..., threads = 0L, use.first.dimnames = TRUE, deparse.level = 1L) {
        merge_BPCellsMatrices(
            list = pack_BPCellsMatrices(...), .fn = rbind2, threads = threads
        )
    }
)

#' @importFrom DelayedArray arbind
#' @export
#' @rdname BPCells-bind
methods::setMethod("arbind", "BPCellsMatrix", function(..., threads = 0L) {
    merge_BPCellsMatrices(
        list = pack_BPCellsMatrices(...), .fn = rbind2, threads = threads
    )
})

#' @param use.names Ignored, always be `TRUE`.
#' @param ignore.mcols Ignored.
#' @param check Ignored.
#' @inheritParams S4Vectors::bindROWS
#' @importFrom DelayedArray bindROWS
#' @export
#' @rdname BPCells-bind
methods::setMethod(
    "bindROWS", "BPCellsMatrix",
    function(x, objects = list(), use.names = TRUE,
             ignore.mcols = TRUE, check = TRUE) {
        assert_(objects, is.list, "a list")
        check_BPCellsMatrices(objects, "objects")
        merge_BPCellsMatrices(list = c(list(x), objects), .fn = rbind2)
    }
)


#################### cbind ########################
#' @importFrom methods cbind2
#' @export
#' @rdname BPCells-bind
methods::setMethod(
    "cbind2", c(x = "BPCellsMatrix", y = "BPCellsMatrix"),
    function(x, y, ..., threads = 0L) {
        DelayedArray(cbind2(x = x@seed, y = y@seed, ..., threads = threads))
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
    function(..., threads = 0L, use.first.dimnames = TRUE, deparse.level = 1L) {
        merge_BPCellsMatrices(
            list = pack_BPCellsMatrices(...), .fn = cbind2, threads = threads
        )
    }
)

#' @importFrom DelayedArray acbind
#' @export
#' @rdname BPCells-bind
methods::setMethod("acbind", "BPCellsMatrix", function(..., threads = 0L, use.first.dimnames = TRUE) {
    merge_BPCellsMatrices(
        list = pack_BPCellsMatrices(...), .fn = cbind2, threads = threads
    )
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
        merge_BPCellsMatrices(list = c(list(x), objects), .fn = cbind2)
    }
)

merge_BPCellsMatrices <- function(list, .fn, ...) {
    Reduce(function(x, y) .fn(x = x, y = y, ...), list)
}

pack_BPCellsMatrices <- function(...) {
    objects <- list(...)
    check_BPCellsMatrices(objects, "...")
    objects
}

check_BPCellsMatrices <- function(lst, arg) {
    BPCellsMatrices <- vapply(lst, methods::is, logical(1L),
        class2 = "BPCellsMatrix"
    )
    if (!all(BPCellsMatrices)) {
        cli::cli_abort(
            c(
                "all input must be a {.cls BPCellsMatrix} object",
                i = "Please check the input {.arg {arg}} in {.val {which(!BPCellsMatrices)}}"
            )
        )
    }
}


#################### BPCellsSeed methods ########################
#' @importFrom methods rbind2
#' @export
#' @rdname BPCells-bind
methods::setMethod(
    "rbind2", c(x = "BPCellsSeed", y = "BPCellsSeed"),
    function(x, y, ..., threads = 0L) {
        fn <- methods::getMethod(
            "rbind2", c("IterableMatrix", "IterableMatrix"),
            where = "BPCells"
        )
        out <- fn(x, y, ...)
        set_threads(BPCellsSeed(out), threads = threads)
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
    function(..., threads = 0L, use.first.dimnames = TRUE, deparse.level = 1L) {
        merge_BPCellsSeeds(
            list = pack_BPCellsSeeds(...), .fn = rbind2, threads = threads
        )
    }
)

#' @importFrom DelayedArray arbind
#' @export
#' @rdname BPCells-bind
methods::setMethod(
    "arbind", "BPCellsSeed",
    function(..., threads = 0L, use.first.dimnames = TRUE) {
        merge_BPCellsSeeds(
            list = pack_BPCellsSeeds(...), .fn = rbind2, threads = threads
        )
    }
)

#' @importFrom DelayedArray bindROWS
#' @export
#' @rdname BPCells-bind
methods::setMethod(
    "bindROWS", "BPCellsSeed",
    function(x, objects = list(), use.names = TRUE,
             ignore.mcols = TRUE, check = TRUE) {
        assert_(objects, is.list, "a list")
        check_BPCellsSeeds(objects, "objects")
        merge_BPCellsSeeds(list = c(list(x), objects), .fn = rbind2)
    }
)

#################### cbind ########################
#' @importFrom methods cbind2
#' @export
#' @rdname BPCells-bind
methods::setMethod(
    "cbind2", c(x = "BPCellsSeed", y = "BPCellsSeed"),
    function(x, y, ..., threads = 0L) {
        fn <- methods::getMethod(
            "cbind2", c("IterableMatrix", "IterableMatrix"),
            where = "BPCells"
        )
        out <- fn(x, y, ...)
        set_threads(BPCellsSeed(out), threads = threads)
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
    function(..., threads = 0L, use.first.dimnames = TRUE, deparse.level = 1L) {
        merge_BPCellsSeeds(
            list = pack_BPCellsSeeds(...), .fn = cbind2, threads = threads
        )
    }
)

#' @importFrom DelayedArray acbind
#' @export
#' @rdname BPCells-bind
methods::setMethod("acbind", "BPCellsSeed", function(..., threads = 0L, use.first.dimnames = TRUE) {
    merge_BPCellsSeeds(
        list = pack_BPCellsSeeds(...),
        .fn = cbind2, threads = threads
    )
})

#' @importFrom S4Vectors bindCOLS
#' @export
#' @rdname BPCells-bind
methods::setMethod(
    "bindCOLS", "BPCellsSeed",
    function(x, objects = list(), use.names = TRUE,
             ignore.mcols = TRUE, check = TRUE) {
        assert_(objects, is.list, "a list")
        check_BPCellsSeeds(objects, "objects")
        merge_BPCellsSeeds(list = c(list(x), objects), .fn = cbind2)
    }
)

merge_BPCellsSeeds <- function(list, .fn, ...) {
    Reduce(function(x, y) .fn(x = x, y = y, ...), list)
}

pack_BPCellsSeeds <- function(...) {
    objects <- list(...)
    check_BPCellsSeeds(objects, "...")
    objects
}

check_BPCellsSeeds <- function(lst, arg) {
    BPCellsSeeds <- vapply(lst, methods::is, logical(1L),
        class2 = "BPCellsSeed"
    )
    if (!all(BPCellsSeeds)) {
        cli::cli_abort(
            c(
                "all input must be a {.cls BPCellsSeed} object",
                i = "Please check the input {.arg {arg}} in {.val {which(!BPCellsSeeds)}}"
            )
        )
    }
}
