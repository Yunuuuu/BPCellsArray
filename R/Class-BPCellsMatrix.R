#' Base Class for Delayed BPCells matrix
#'
#' The `BPCellsMatrix` class just inherits from the
#' [DelayedMatrix][DelayedArray::DelayedMatrix] object. The purpose for
#' `BPCellsMatrix` object is to provide the common methods for all Delayed
#' BPCells matrix.

#' @param ...
#'  - `rbind` and `arbind`: A list of `BPCellsMatrix` objects.
#'  - `rbind2` and `[`: Not used currently.
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

#' @inheritParams BPCellsSeed-Class
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
#' - `dimnames<-`: A [BPCellsRenameDimsMatrix][BPCellsRenameDims] object.
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
#' - `x %*% y`: Matrix multiplication, a [BPCellsMatrix] object or a dense
#'   matrix (matrix and numeric methods).
#' @importMethodsFrom DelayedArray %*%
#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "%*%", c(x = "BPCellsMatrix", y = "BPCellsMatrix"), function(x, y) {
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
#' - `crossprod(x, y)`: Matrix Crossproduct, a [BPCellsMatrix] object or a dense
#'   matrix (matrix and numeric methods).
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
#' @return
#' - `cbind2`, `acbind`, `cbind`, `bindCOLS`: A
#'   [BPCellsCOLSBindMatrixMatrix][BPCellsBindMatrix] object.
#' - `rbind2`, `arbind`, `rbind`, `bindROWS`: A
#'   [BPCellsRowBindMatrixMatrix][BPCellsBindMatrix] object.
#' @importMethodsFrom methods rbind2
#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "rbind2", c(x = "BPCellsMatrix", y = "BPCellsMatrix"),
    function(x, y, ..., threads = 0L) {
        DelayedArray(rbind2(x = x@seed, y = y@seed, ..., threads = threads))
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

#' @param use.first.dimnames Ignored, always be `TRUE` in BPCells.
#' @param deparse.level Ignored, used by generic methods.
#' @importMethodsFrom DelayedArray rbind
#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "rbind", "BPCellsMatrix",
    function(..., threads = 0L, use.first.dimnames = TRUE, deparse.level = 1L) {
        merge_BPCellsMatrices(
            list = pack_BPCellsMatrices(...), .fn = rbind2, threads = threads
        )
    }
)

#' @importMethodsFrom DelayedArray arbind
#' @export
#' @rdname BPCellsMatrix
methods::setMethod("arbind", "BPCellsMatrix", function(..., threads = 0L) {
    merge_BPCellsMatrices(
        list = pack_BPCellsMatrices(...), .fn = rbind2, threads = threads
    )
})

#' @param use.names Ignored, always be `TRUE`.
#' @param ignore.mcols Ignored.
#' @param check Ignored.
#' @importMethodsFrom DelayedArray bindROWS
#' @export
#' @rdname BPCellsMatrix
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
#' @importMethodsFrom methods cbind2
#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "cbind2", c(x = "BPCellsMatrix", y = "BPCellsMatrix"),
    function(x, y, ..., threads = 0L) {
        DelayedArray(cbind2(x = x@seed, y = y@seed, ..., threads = threads))
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

#' @importMethodsFrom DelayedArray cbind
#' @export
#' @rdname BPCellsMatrix
methods::setMethod(
    "cbind", "BPCellsMatrix",
    function(..., threads = 0L, use.first.dimnames = TRUE, deparse.level = 1L) {
        merge_BPCellsMatrices(
            list = pack_BPCellsMatrices(...), .fn = cbind2, threads = threads
        )
    }
)

#' @importMethodsFrom DelayedArray acbind
#' @export
#' @rdname BPCellsMatrix
methods::setMethod("acbind", "BPCellsMatrix", function(..., threads = 0L, use.first.dimnames = TRUE) {
    merge_BPCellsMatrices(
        list = pack_BPCellsMatrices(...), .fn = cbind2, threads = threads
    )
})

#' @importMethodsFrom S4Vectors bindCOLS
#' @export
#' @rdname BPCellsMatrix
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
