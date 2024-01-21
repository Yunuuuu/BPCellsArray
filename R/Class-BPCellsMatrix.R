#' Base Class for Delayed BPCells matrix
#'
#' The `BPCellsMatrix` class just inherits from the
#' [DelayedMatrix][DelayedArray::DelayedMatrix] object. The purpose for
#' `BPCellsMatrix` object is to provide the common methods for all Delayed
#' BPCells matrix.
#'
#' @importClassesFrom DelayedArray DelayedMatrix
#' @export
#' @name BPCellsMatrix
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


#' @param x A [BPCellsMatrix] object. For following methods:
#'  - `%*%`, and `crossprod`: A [BPCellsMatrix] object or matrx-like object
#'     which can be coerced into a [dgCMatrix][Matrix::dgCMatrix-class].
#' @return
#' - `t`: A [BPCellsMatrix] object.
#' @importMethodsFrom DelayedArray t
#' @export
#' @rdname BPCellsMatrix
methods::setMethod("t", "BPCellsMatrix", function(x) {
    DelayedArray(t(x@seed))
})

#' @param i,j Row and Column index.
#' @param drop Ignored, always be `FALSE`.
#' @param ... Ignored currently.
#' @return
#' - `[`: A [BPCellsMatrix] object.
#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "[", "BPCellsMatrix",
    function(x, i, j, ..., drop = FALSE) {
        DelayedArray(x@seed[i, j, ...])
    }
)

#' @inheritParams BPCellsSeed
#' @return
#' - `[<-`: A [BPCellsMatrix] object.
#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "[<-", "BPCellsMatrix",
    function(x, i, j, ..., value) {
        seed <- x@seed
        seed[i, j, ...] <- value
        DelayedArray(seed)
    }
)

methods::setClassUnion("ListOrNULL", c("list", "NULL"))

# for `dim`, `dimnames`, `extract_array` and `is_sparse` just use the methods
# from DelayedArray
#' @return
#' - `dimnames<-`: A [BPCellsMatrix] object, usually a `BPCellsRenameDimsMatrix`
#'   object.
#' @importMethodsFrom DelayedArray dimnames<-
#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "dimnames<-",
    c(x = "BPCellsMatrix", value = "ListOrNULL"), function(x, value) {
        seed <- x@seed
        dimnames(seed) <- value
        DelayedArray(seed)
    }
)

#################### Matrix Statistics ########################
#' @importMethodsFrom DelayedArray rowSums
#' @return * `rowSums()`: vector of row sums
#' @export
#' @rdname BPCellsMatrix
methods::setMethod("rowSums", c(x = "BPCellsMatrix"), function(x) {
    rowSums(x@seed)
})

#' @importMethodsFrom DelayedArray colSums
#' @return * `colSums()`: vector of col sums
#' @export
#' @rdname BPCellsMatrix
methods::setMethod("colSums", c(x = "BPCellsMatrix"), function(x) {
    colSums(x@seed)
})

#' @importMethodsFrom DelayedArray rowMeans
#' @return * `rowMeans()`: vector of row means
#' @export
#' @rdname BPCellsMatrix
methods::setMethod("rowMeans", c(x = "BPCellsMatrix"), function(x) {
    rowMeans(x@seed)
})

#' @importMethodsFrom DelayedArray colMeans
#' @return * `colMeans()`: vector of col means
#' @export
#' @rdname BPCellsMatrix
methods::setMethod("colMeans", c(x = "BPCellsMatrix"), function(x) {
    colMeans(x@seed)
})

######################################################################
#' @param y A [BPCellsMatrix] object or matrx-like object which can be coerced
#'   into a [dgCMatrix][Matrix::dgCMatrix-class].
#' @return
#' - `x %*% y`: Matrix multiplication, a [BPCellsSeed] object or a dense
#'   matrix (matrix and numeric methods).
#' @importMethodsFrom DelayedArray %*%
#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "%*%",
    c(x = "BPCellsMatrix", y = "BPCellsMatrix"), function(x, y) {
        DelayedArray(x@seed %*% y@seed)
    }
)

#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "%*%", c(x = "BPCellsMatrix", y = "dgCMatrix"), function(x, y) {
        DelayedArray(x@seed %*% y)
    }
)

#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "%*%", c(x = "dgCMatrix", y = "BPCellsMatrix"), function(x, y) {
        DelayedArray(x %*% y@seed)
    }
)

#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "%*%", c(x = "BPCellsMatrix", y = "ANY"), function(x, y) {
        DelayedArray(x@seed %*% methods::as(y, "dgCMatrix"))
    }
)

#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "%*%", c(x = "ANY", y = "BPCellsMatrix"), function(x, y) {
        DelayedArray(methods::as(x, "dgCMatrix") %*% y@seed)
    }
)

#################### Matrix multiplication ########################
# following methods return dense matrix
#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "%*%", c(x = "BPCellsMatrix", y = "matrix"), function(x, y) {
        x@seed %*% y
    }
)

#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "%*%", c(x = "matrix", y = "BPCellsMatrix"), function(x, y) {
        x %*% y@seed
    }
)

#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "%*%", c(x = "BPCellsMatrix", y = "numeric"), function(x, y) {
        x@seed %*% y
    }
)

#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "%*%", c(x = "numeric", y = "BPCellsMatrix"), function(x, y) {
        x %*% y@seed
    }
)

#################### Matrix Crossproduct ########################
#' @importMethodsFrom DelayedArray crossprod
#' @return
#' - `crossprod(x, y)`: Matrix Crossproduct, a [BPCellsSeed] object or a dense
#'   matrix (matrix and numeric methods).
#'   object
#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "crossprod", c(x = "BPCellsMatrix", y = "BPCellsMatrix"), function(x, y) {
        t(x) %*% y
    }
)

#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "crossprod", c(x = "BPCellsMatrix", y = "dgCMatrix"), function(x, y) {
        t(x) %*% y
    }
)

#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "crossprod", c(x = "dgCMatrix", y = "BPCellsMatrix"), function(x, y) {
        t(x) %*% y
    }
)

#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "crossprod", c(x = "BPCellsMatrix", y = "ANY"), function(x, y) {
        t(x) %*% methods::as(y, "dgCMatrix")
    }
)

#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "crossprod", c(x = "ANY", y = "BPCellsMatrix"), function(x, y) {
        t(methods::as(x, "dgCMatrix")) %*% y
    }
)

#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "crossprod", c(x = "BPCellsMatrix", y = "matrix"), function(x, y) {
        t(x) %*% y
    }
)

#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "crossprod", c(x = "matrix", y = "BPCellsMatrix"), function(x, y) {
        t(x) %*% y
    }
)

#################### rbind ########################
#' @param threads Set number of threads to use for sparse-dense multiply and
#' [matrix_stats][BPCells::matrix_stats].
#' @importMethodsFrom methods rbind2
#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "rbind2", c(x = "BPCellsMatrix", y = "BPCellsMatrix"),
    function(x, y, ..., threads = 0L) {
        threads <- as.integer(max(0L, threads, na.rm = TRUE))
        out <- methods::rbind2(x@seed, y@seed, ...)
        out@threads <- threads
        BPCellsRowBindMatrixArray(out)
    }
)

#' @export
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
methods::setMethod(
    "rbind2", c(x = "BPCellsMatrix", y = "ANY"),
    function(x, y, ...) {
        cli::cli_abort(c(
            "Cannot combine {.cls {obj_type_friendly(x)}} object with a {.cls {obj_type_friendly(y)}}",
            i = "{.arg y} must be a {.cls BPCellsMatrix} object"
        ))
    }
)

#' @importMethodsFrom S4Arrays arbind
#' @export
#' @rdname BPCellsMatrix
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
#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "rbind", "BPCellsMatrix",
    function(..., threads = 0L, use.first.dimnames = TRUE, deparse.level = 1L) {
        arbind(..., threads = threads)
    }
)

#################### cbind ########################
#' @importMethodsFrom methods cbind2
#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "cbind2", c(x = "BPCellsMatrix", y = "BPCellsMatrix"),
    function(x, y, ..., threads = 0L) {
        threads <- as.integer(max(0L, threads, na.rm = TRUE))
        out <- methods::cbind2(x@seed, y@seed, ...)
        out@threads <- threads
        BPCellsColBindMatrixArray(out)
    }
)

#' @export
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
methods::setMethod(
    "cbind2", c(x = "BPCellsMatrix", y = "ANY"),
    function(x, y, ...) {
        cli::cli_abort(c(
            "Cannot combine {.cls {obj_type_friendly(x)}} object with a {.cls {obj_type_friendly(y)}}",
            i = "{.arg y} must be a {.cls BPCellsMatrix} object"
        ))
    }
)

#' @importMethodsFrom S4Arrays acbind
#' @export
#' @rdname BPCellsMatrix
methods::setMethod("acbind", "BPCellsMatrix", function(..., threads = 0L, use.first.dimnames = TRUE) {
    threads <- as.integer(max(0L, threads, na.rm = TRUE))
    lst <- pack_BPCellsMatrices(...)
    out <- Reduce(function(x, y) cbind2(x, y), lst)
    out@threads <- threads
    BPCellsRowBindMatrixArray(out)
})

#' @importMethodsFrom DelayedArray cbind
#' @export
#' @rdname BPCellsMatrix
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
