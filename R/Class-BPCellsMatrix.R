#' Base Class for Delayed BPCells matrix
#'
#' The `BPCellsMatrix` class just inherits from the
#' [DelayedMatrix][DelayedArray::DelayedMatrix] object. The purpose for
#' `BPCellsMatrix` object is to provide the common methods for all Delayed
#' BPCells matrix.
#'
#' @name BPCellsMatrix
#' @importClassesFrom DelayedArray DelayedMatrix
#' @export
#' @include Class-BPCellsSeed.R
methods::setClass("BPCellsMatrix",
    contains = "DelayedMatrix",
    slots = list(seed = "BPCellsSeed")
)

#' @param object A `BPCellsMatrix` object.
#' @importMethodsFrom methods show
#' @export
#' @rdname BPCellsMatrix
methods::setMethod("show", "BPCellsMatrix", function(object) {
    show_bpcells(object@seed, "DelayedMatrix", class(object))
})

#' @importMethodsFrom DelayedArray t
#' @export
#' @rdname BPCellsMatrix
methods::setMethod("t", "BPCellsMatrix", function(x) {
    DelayedArray(t(x@seed))
})

methods::setMethod(
    "[", "BPCellsMatrix",
    function(x, i, j, ..., drop = FALSE) {
        DelayedArray(x@seed[i, j, ...])
    }
)

methods::setMethod(
    "[<-", "BPCellsMatrix",
    function(x, i, j, ..., value) {
        seed <- x@seed
        seed[i, j, ...] <- value
        DelayedArray(seed)
    }
)

# for `dim`, `dimnames`, `extract_array` and `is_sparse` just use the methods
# from DelayedArray
#' @importMethodsFrom DelayedArray dimnames<-
methods::setMethod(
    "dimnames<-",
    signature(x = "BPCellsMatrix", value = "ListOrNULL"), function(x, value) {
        seed <- x@seed
        dimnames(seed) <- value
        DelayedArray(seed)
    }
)

#################### Matrix products ########################
#' @importMethodsFrom DelayedArray %*%
#' @return
#' - `x %*% y`: Matrix products, a [BPCellsMatrixMultiplyMatrix] object
#' @name BPCellsMatrix
methods::setMethod(
    "%*%",
    signature(x = "BPCellsMatrix", y = "BPCellsMatrix"), function(x, y) {
        BPCellsMultiplyArray(x@seed %*% y@seed)
    }
)

methods::setMethod(
    "%*%", signature(x = "BPCellsMatrix", y = "dgCMatrix"), function(x, y) {
        BPCellsMultiplyArray(x@seed %*% y)
    }
)

methods::setMethod(
    "%*%", signature(x = "dgCMatrix", y = "BPCellsMatrix"), function(x, y) {
        BPCellsMultiplyArray(x %*% y@seed)
    }
)

methods::setMethod(
    "%*%", signature(x = "BPCellsMatrix", y = "ANY"), function(x, y) {
        BPCellsMultiplyArray(x@seed %*% methods::as(y, "dgCMatrix"))
    }
)

methods::setMethod(
    "%*%", signature(x = "ANY", y = "BPCellsMatrix"), function(x, y) {
        BPCellsMultiplyArray(methods::as(x, "dgCMatrix") %*% y@seed)
    }
)

# following methods return dense matrix
methods::setMethod(
    "%*%",
    signature(x = "BPCellsMatrix", y = "matrix"), function(x, y) {
        x@seed %*% y
    }
)

methods::setMethod(
    "%*%",
    signature(x = "matrix", y = "BPCellsMatrix"), function(x, y) {
        x %*% y@seed
    }
)

methods::setMethod(
    "%*%",
    signature(x = "BPCellsMatrix", y = "numeric"), function(x, y) {
        x@seed %*% y
    }
)

methods::setMethod(
    "%*%",
    signature(x = "numeric", y = "BPCellsMatrix"), function(x, y) {
        x %*% y@seed
    }
)

#################### Matrix Crossproduct ########################
#' @importMethodsFrom DelayedArray crossprod
#' @return
#' - `crossprod(x, y)`: Matrix Crossproduct, a [BPCellsMatrixMultiplyMatrix]
#'   object
methods::setMethod(
    "crossprod",
    signature(x = "BPCellsMatrix", y = "BPCellsMatrix"), function(x, y) {
        t(x) %*% y
    }
)

methods::setMethod(
    "crossprod",
    signature(x = "BPCellsMatrix", y = "dgCMatrix"), function(x, y) {
        t(x) %*% y
    }
)

methods::setMethod(
    "crossprod",
    signature(x = "dgCMatrix", y = "BPCellsMatrix"), function(x, y) {
        t(x) %*% y
    }
)

methods::setMethod(
    "crossprod",
    signature(x = "BPCellsMatrix", y = "matrix"), function(x, y) {
        t(x) %*% y
    }
)

methods::setMethod(
    "crossprod",
    signature(x = "matrix", y = "BPCellsMatrix"), function(x, y) {
        t(x) %*% y
    }
)

methods::setMethod(
    "crossprod",
    signature(x = "BPCellsMatrix", y = "ANY"), function(x, y) {
        t(x) %*% methods::as(y, "dgCMatrix")
    }
)

methods::setMethod(
    "crossprod", signature(x = "ANY", y = "BPCellsMatrix"), function(x, y) {
        t(methods::as(x, "dgCMatrix")) %*% y
    }
)

#################### Matrix Statistics ########################
#' @importMethodsFrom DelayedArray rowSums
#' @return * `rowSums()`: vector of row sums
methods::setMethod("rowSums", signature(x = "BPCellsMatrix"), function(x) {
    rowSums(x@seed)
})

#' @importMethodsFrom DelayedArray colSums
#' @return * `colSums()`: vector of col sums
methods::setMethod("colSums", signature(x = "BPCellsMatrix"), function(x) {
    colSums(x@seed)
})

#' @importMethodsFrom DelayedArray rowMeans
#' @return * `rowMeans()`: vector of row means
methods::setMethod("rowMeans", signature(x = "BPCellsMatrix"), function(x) {
    rowMeans(x@seed)
})

#' @importMethodsFrom DelayedArray colMeans
#' @return * `colMeans()`: vector of col means
methods::setMethod("colMeans", signature(x = "BPCellsMatrix"), function(x) {
    colMeans(x@seed)
})

#################### rbind ########################
#' @importMethodsFrom methods rbind2
methods::setMethod(
    "rbind2", signature(x = "BPCellsMatrix", y = "BPCellsMatrix"),
    function(x, y, ..., threads = 0L) {
        threads <- as.integer(max(0L, threads, na.rm = TRUE))
        out <- methods::rbind2(x@seed, y@seed, ...)
        out@threads <- threads
        BPCellsRowBindMatrixArray(out)
    }
)

methods::setMethod(
    "rbind2", signature(x = "ANY", y = "BPCellsMatrix"),
    function(x, y, ...) {
        cli::cli_abort(c(
            "Cannot combine {.cls {obj_type_friendly(x)}} object with a {.cls {obj_type_friendly(y)}}",
            i = "{.arg x} must be a {.cls BPCellsMatrix} object"
        ))
    }
)

methods::setMethod(
    "rbind2", signature(x = "BPCellsMatrix", y = "ANY"),
    function(x, y, ...) {
        cli::cli_abort(c(
            "Cannot combine {.cls {obj_type_friendly(x)}} object with a {.cls {obj_type_friendly(y)}}",
            i = "{.arg y} must be a {.cls BPCellsMatrix} object"
        ))
    }
)

#' @importMethodsFrom S4Arrays arbind
methods::setMethod("arbind", "BPCellsMatrix", function(..., threads = 0L, use.first.dimnames = TRUE) {
    threads <- as.integer(max(0L, threads, na.rm = TRUE))
    lst <- pack_BPCellsMatrices(...)
    out <- Reduce(function(x, y) rbind2(x, y), lst)
    out@threads <- threads
    BPCellsRowBindMatrixArray(out)
})

#' @param use.first.dimnames Ignored, always be `TRUE` in BPCells.
#' @param deparse.level Ignored, used by generic methods.
#' @importMethodsFrom DelayedArray rbind
methods::setMethod(
    "rbind", "BPCellsMatrix",
    function(..., threads = 0L, use.first.dimnames = TRUE, deparse.level = 1L) {
        arbind(..., threads = threads)
    }
)

#################### cbind ########################
#' @importMethodsFrom methods cbind2
methods::setMethod(
    "cbind2", signature(x = "BPCellsMatrix", y = "BPCellsMatrix"),
    function(x, y, ..., threads = 0L) {
        threads <- as.integer(max(0L, threads, na.rm = TRUE))
        out <- methods::cbind2(x@seed, y@seed, ...)
        out@threads <- threads
        BPCellsColBindMatrixArray(out)
    }
)

methods::setMethod(
    "cbind2", signature(x = "ANY", y = "BPCellsMatrix"),
    function(x, y, ...) {
        cli::cli_abort(c(
            "Cannot combine {.cls {obj_type_friendly(x)}} object with a {.cls {obj_type_friendly(y)}}",
            i = "{.arg x} must be a {.cls BPCellsMatrix} object"
        ))
    }
)

methods::setMethod(
    "cbind2", signature(x = "BPCellsMatrix", y = "ANY"),
    function(x, y, ...) {
        cli::cli_abort(c(
            "Cannot combine {.cls {obj_type_friendly(x)}} object with a {.cls {obj_type_friendly(y)}}",
            i = "{.arg y} must be a {.cls BPCellsMatrix} object"
        ))
    }
)

#' @importMethodsFrom S4Arrays acbind
methods::setMethod("acbind", "BPCellsMatrix", function(..., threads = 0L, use.first.dimnames = TRUE) {
    threads <- as.integer(max(0L, threads, na.rm = TRUE))
    lst <- pack_BPCellsMatrices(...)
    out <- Reduce(function(x, y) cbind2(x, y), lst)
    out@threads <- threads
    BPCellsRowBindMatrixArray(out)
})

#' @importMethodsFrom DelayedArray cbind
methods::setMethod(
    "cbind", "BPCellsMatrix",
    function(..., threads = 0L, use.first.dimnames = TRUE, deparse.level = 1L) {
        acbind(..., deparse.level = 1L, use.first.dimnames = TRUE)
    }
)

pack_BPCellsMatrices <- function(...) {
    objects <- list(...)
    BPCellsMatrices <- vapply(objects, methods::is,
        logical(1L),
        class = "BPCellsMatrix"
    )
    if (!all(BPCellsMatrices)) {
        cli::cli_abort(c(
            "all input must be a {.cls BPCellsMatrix} object",
            i = "Please check the input in {.val {which(!BPCellsMatrices)}}"
        ))
    }
    lapply(objects, methods::slot, name = "seed")
}
