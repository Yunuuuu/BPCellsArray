#' Delayed BPCells ColBindMatrices and RowBindMatrices
#'
#' @description
#' The `BPCellsRowBindMatrixArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `RowBindMatrices`
#' object in BPCells.
#'
#' The `BPCellsColBindMatrixArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `ColBindMatrices`
#' object in BPCells.
#'
#' @note
#' Usually, you shouldn't use this class directly, instead, you should use
#' [cbind2][methods::cbind2] ([cbind][DelayedArray::cbind],
#' [acbind][S4Arrays::acbind], [bindCOLS][S4Vectors::bindCOLS]) or
#' [rbind2][methods::rbind2] ([rbind][DelayedArray::rbind],
#' [arbind][S4Arrays::arbind], [bindROWS][S4Vectors::bindROWS]) to create a
#' `BPCellsColBindMatrixMatrix` or `BPCellsRowBindMatrixMatrix` object.
#'
#' @param x For Specific functions:
#' - `BPCellsColBindMatrixArray` and `BPCellsRowBindMatrixArray`: A
#'   `ColBindMatrices` or `RowBindMatrices` object.
#' - `matrixClass`: A `BPCellsColBindMatrixArray` or `BPCellsRowBindMatrixArray`
#'   object.
#' @seealso [BPCellsSeed]
#' @name BPCellsBindMatrix-Class
NULL

methods::setClass("BPCellsColBindMatrixSeed",
    contains = c("BPCellsSeed", get_class("ColBindMatrices"))
)

methods::setValidity("BPCellsColBindMatrixSeed", function(object) {
    BPCellsSeeds <- vapply(object@matrix_list,
        methods::is, logical(1L),
        class = "BPCellsSeed"
    )
    if (!all(BPCellsSeeds)) {
        cli::cli_abort(c(
            "all `@matrix_list` must be a {.cls BPCellsSeed} object",
            i = "Please check the `@matrix_list` in {.val {which(!BPCellsMatrices)}}"
        ))
    }
    return(TRUE)
})

#' @return
#' - `BPCellsColBindMatrixSeed`: A `BPCellsColBindMatrixSeed` object.
#' - `BPCellsRowBindMatrixSeed`: A `BPCellsRowBindMatrixSeed` object.
#' @rdname BPCellsBindMatrix-Class
#' @noRd
BPCellsColBindMatrixSeed <- function(x) {
    assert_s4_class(x, "ColBindMatrices")
    x@matrix_list <- lapply(x@matrix_list, BPCellsSeed)
    methods::as(x, "BPCellsColBindMatrixSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsBindMatrix-Class
methods::setClass("BPCellsColBindMatrixArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsColBindMatrixSeed")
)

#' @param seed A `BPCellsColBindMatrixSeed` or `BPCellsRowBindMatrixSeed`
#' object.
#' @return
#' - `DelayedArray`: A `BPCellsColBindMatrixMatrix` or
#'   `BPCellsRowBindMatrixMatrix` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @export
#' @rdname BPCellsBindMatrix-Class
methods::setMethod(
    "DelayedArray", "BPCellsColBindMatrixSeed",
    function(seed) new_DelayedArray(seed, Class = "BPCellsColBindMatrixArray")
)

#' @return
#' - `BPCellsColBindMatrixArray`: A `BPCellsColBindMatrixMatrix` object.
#' - `BPCellsColBindMatrixArray`: A `BPCellsRowBindMatrixMatrix` object.
#' @export
#' @rdname BPCellsBindMatrix-Class
BPCellsColBindMatrixArray <- function(x) {
    DelayedArray(BPCellsColBindMatrixSeed(x))
}

#' @export
#' @rdname BPCellsBindMatrix-Class
methods::setClass("BPCellsColBindMatrixMatrix",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsColBindMatrixSeed")
)

#' @export
#' @importMethodsFrom DelayedArray matrixClass
#' @rdname BPCellsBindMatrix-Class
methods::setMethod("matrixClass", "BPCellsColBindMatrixArray", function(x) {
    "BPCellsColBindMatrixMatrix"
})

###################################################################
###########################  Methods  #############################
###################################################################


#' @param ... Ignored, Not used curretly.
#' @inheritParams BPCellsMatrix-Class
#' @importMethodsFrom BPCells [
#' @rdname internal-methods
methods::setMethod(
    "[", "BPCellsColBindMatrixSeed",
    function(x, i, j, ..., drop = FALSE) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @param object A `BPCellsColBindMatrixSeed` or `BPCellsRowBindMatrixSeed`
#' object.
#' @return
#' - `path`: A character file paths.
#' @importMethodsFrom DelayedArray path
#' @rdname BPCellsBindMatrix-Class
#' @noRd
methods::setMethod("path", "BPCellsColBindMatrixSeed", function(object) {
    unlist(lapply(object@matrix_list, path),
        recursive = FALSE, use.names = FALSE
    )
})

############################################################
############################################################
############################################################
############################################################
############################################################
#' @rdname BPCellsBindMatrix-Class
#' @noRd
methods::setClass("BPCellsRowBindMatrixSeed",
    contains = c("BPCellsSeed", get_class("RowBindMatrices"))
)

methods::setValidity("BPCellsRowBindMatrixSeed", function(object) {
    BPCellsSeeds <- vapply(object@matrix_list,
        methods::is, logical(1L),
        class = "BPCellsSeed"
    )
    if (!all(BPCellsSeeds)) {
        cli::cli_abort(c(
            "all `@matrix_list` must be a {.cls BPCellsSeed} object",
            i = "Please check the `@matrix_list` in {.val {which(!BPCellsMatrices)}}"
        ))
    }
    return(TRUE)
})

#' @rdname BPCellsBindMatrix-Class
#' @noRd
BPCellsRowBindMatrixSeed <- function(x) {
    assert_s4_class(x, "RowBindMatrices")
    x@matrix_list <- lapply(x@matrix_list, BPCellsSeed)
    methods::as(x, "BPCellsRowBindMatrixSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsBindMatrix-Class
methods::setClass("BPCellsRowBindMatrixArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsRowBindMatrixSeed")
)

#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @export
#' @rdname BPCellsBindMatrix-Class
methods::setMethod(
    "DelayedArray", "BPCellsRowBindMatrixSeed",
    function(seed) new_DelayedArray(seed, Class = "BPCellsRowBindMatrixArray")
)

#' @export
#' @rdname BPCellsBindMatrix-Class
BPCellsRowBindMatrixArray <- function(x) {
    DelayedArray(BPCellsRowBindMatrixSeed(x))
}

#' @export
#' @rdname BPCellsBindMatrix-Class
methods::setClass("BPCellsRowBindMatrixMatrix",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsRowBindMatrixSeed")
)

#' @export
#' @importMethodsFrom DelayedArray matrixClass
#' @rdname BPCellsBindMatrix-Class
methods::setMethod("matrixClass", "BPCellsRowBindMatrixArray", function(x) {
    "BPCellsRowBindMatrixMatrix"
})

###################################################################
###########################  Methods  #############################
###################################################################

#' @importMethodsFrom BPCells [
#' @rdname internal-methods
methods::setMethod(
    "[", "BPCellsRowBindMatrixSeed",
    function(x, i, j, ..., drop = FALSE) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @importMethodsFrom DelayedArray path
#' @rdname BPCellsBindMatrix-Class
#' @noRd
methods::setMethod("path", "BPCellsRowBindMatrixSeed", function(object) {
    unlist(lapply(object@matrix_list, path),
        recursive = FALSE, use.names = FALSE
    )
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
    "set_threads", "BPCellsRowBindMatrixSeed",
    function(object, threads = 0L) {
        threads <- as.integer(max(0L, threads, na.rm = TRUE))
        object@threads <- threads
        object
    }
)

#' @export
#' @rdname set_threads
methods::setMethod(
    "set_threads", "BPCellsColBindMatrixSeed",
    function(object, threads = 0L) {
        threads <- as.integer(max(0L, threads, na.rm = TRUE))
        object@threads <- threads
        object
    }
)

#' @export
#' @rdname set_threads
methods::setMethod(
    "set_threads", "BPCellsRowBindMatrixMatrix",
    function(object, threads = 0L) {
        object@seed <- set_threads(object@seed, threads = threads)
        object
    }
)

#' @export
#' @rdname set_threads
methods::setMethod(
    "set_threads", "BPCellsColBindMatrixMatrix",
    function(object, threads = 0L) {
        object@seed <- set_threads(object@seed, threads = threads)
        object
    }
)
