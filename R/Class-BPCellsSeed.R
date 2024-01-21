#' Low-level Base Class for Delayed BPCells matrix
#'
#' The `BPCellsSeed` class just inherits from the `IterableMatrix` object in
#' BPCells package. The purpose for `BPCellsSeed` object is to provide the
#' common methods for all low-level BPCells seed objects.
#'
#' @param x A [BPCellsSeed] object. For some functions:
#'  - `BPCellsSeed`: A [BPCellsSeed] object, or other BPCells `IterableMatrix`
#'    object.
#'  - `%*%`, and `crossprod`: A [BPCellsSeed] object or matrx-like object which
#'     can be coerced into a [dgCMatrix][Matrix::dgCMatrix-class].
#' @param y A [BPCellsSeed] object or matrx-like object which can be coerced
#'   into a [dgCMatrix][Matrix::dgCMatrix-class].
#' @param ... Additional arguments passed to specific methods.
#' @export
#' @name BPCellsSeed-Class
methods::setClass("BPCellsSeed", contains = get_class("IterableMatrix"))

#' @param object A `BPCellsSeed` object.
#' @importMethodsFrom methods show
#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod("show", "BPCellsSeed", function(object) {
    show_bpcells(object, "BPCellsSeed", class(object))
})

#' @return
#' - `type`: A string. For all BPCells matrix type of `float` and `double`,
#'   always return `double` since R cannot differentiate 32-bit and 64-bit real
#'   number.
#' @importMethodsFrom DelayedArray type
#' @export
#' @rdname BPCellsSeed-Class
#' @include utils.R
methods::setMethod("type", "BPCellsSeed", function(x) {
    switch(BPCells:::matrix_type(x),
        uint32_t = "integer",
        float = "double",
        double = "double"
    )
})

#' @return
#' - `is_sparse`: Always return `TRUE` for `BPCellsSeed` object.
#' @importMethodsFrom DelayedArray is_sparse
#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod("is_sparse", "BPCellsSeed", function(x) TRUE)

#' @inheritParams S4Arrays::extract_array
#' @return
#' - `extract_array`: A dense matrix.
#' @importMethodsFrom DelayedArray extract_array
#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "extract_array", "BPCellsSeed",
    function(x, index) {
        out <- as.matrix(extract_bpcells_array(x, index))
        storage.mode(out) <- type(x)
        out
    }
)

#' @return
#' - `extract_sparse_array`: A [SparseArraySeed][DelayedArray::SparseArraySeed]
#'   object.
#' @importMethodsFrom DelayedArray extract_sparse_array
#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "extract_sparse_array", "BPCellsSeed",
    function(x, index) {
        methods::as(extract_bpcells_array(x, index), "SparseArraySeed")
    }
)


###################################################################
# All delayed operations should be wrapped into a `BPCellsSeed` object
# In BPCells, `dimnames<-` was only defined for `IterableMatrix`.
# `dimnames<-` return another `IterableMatrix` object.
# we wrap it into a `BPCellsSeed` object.
#' @param value
#'  - `dimnames<-`: A list of dimnames or `NULL`.
#'  - `[<-`: A matrix which can be coerced into
#'     [dgCMatrix][Matrix::dgCMatrix-class].
#'  - `pmin_scalar`: Single positive numeric value.
#' @param values A positive atomic numeric.
#' @return
#' - `dimnames<-`: A [BPCellsSeed] object, usually a `BPCellsRenameDimsSeed`
#'   object.
#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "dimnames<-",
    c(x = "BPCellsSeed", value = "list"), function(x, value) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "dimnames<-",
    c(x = "BPCellsSeed", value = "NULL"), function(x, value) {
        BPCellsSeed(methods::callNextMethod())
    }
)

# t will not change the underlying class
#' @importMethodsFrom DelayedArray t
#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod("t", "BPCellsSeed", function(x) methods::callNextMethod())

# In BPCells, `[<-` was only defined for `IterableMatrix`
#' @param i,j Row and Column index.
#' @return
#' - `[<-`: A [BPCellsSeed] object.
#' @importMethodsFrom BPCells [<-
#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "[<-", "BPCellsSeed", function(x, i, j, ..., value) {
        BPCellsSeed(methods::callNextMethod())
    }
)

###################### Matrix multiplication ########################
#' @importMethodsFrom DelayedArray %*%
#' @return
#' - `x %*% y`: matrix multiplication, a [BPCellsSeed] object or a dense
#'   matrix (matrix and numeric methods).
#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "%*%", c(x = "BPCellsSeed", y = "BPCellsSeed"), function(x, y) {
        if (x@transpose != y@transpose) {
            if (x@transpose) {
                cli::cli_warn(
                    "{.arg x} is transposed but {.arg y} not, transposing the storage order for {.arg x}" # nolint
                )
                x <- BPCells::transpose_storage_order(x)
            } else {
                cli::cli_warn(
                    "{.arg y} is transposed but {.arg x} not, transposing the storage order for {.arg y}" # nolint
                )
                y <- BPCells::transpose_storage_order(y)
            }
        }
        fn <- methods::getMethod(
            "%*%",
            c("IterableMatrix", "IterableMatrix"),
            where = "BPCells"
        )
        BPCellsSeed(fn(x, y))
    }
)

#' @importClassesFrom Matrix dgCMatrix
#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "%*%", c(x = "BPCellsSeed", y = "dgCMatrix"), function(x, y) {
        fn <- methods::getMethod(
            "%*%", c("IterableMatrix", "dgCMatrix"),
            where = "BPCells"
        )
        BPCellsSeed(fn(x, y))
    }
)

#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "%*%", c(x = "dgCMatrix", y = "BPCellsSeed"), function(x, y) {
        fn <- methods::getMethod(
            "%*%", c("dgCMatrix", "IterableMatrix"),
            where = "BPCells"
        )
        BPCellsSeed(fn(x, y))
    }
)

#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "%*%", c(x = "BPCellsSeed", y = "ANY"), function(x, y) {
        x %*% coerce_dgCMatrix(y)
    }
)

#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "%*%", c(x = "ANY", y = "BPCellsSeed"), function(x, y) {
        coerce_dgCMatrix(x) %*% y
    }
)

#################### Matrix multiplication ########################
# following methods return a dense matrix
#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "%*%", c(x = "BPCellsSeed", y = "matrix"), function(x, y) {
        fn <- methods::getMethod(
            "%*%", c("IterableMatrix", "matrix"),
            where = "BPCells"
        )
        storage.mode(y) <- "double"
        fn(x, y)
    }
)

#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "%*%", c(x = "matrix", y = "BPCellsSeed"), function(x, y) {
        fn <- methods::getMethod(
            "%*%", c("matrix", "IterableMatrix"),
            where = "BPCells"
        )
        storage.mode(x) <- "double"
        fn(x, y)
    }
)

#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "%*%", c(x = "BPCellsSeed", y = "numeric"), function(x, y) {
        fn <- methods::getMethod(
            "%*%", c("IterableMatrix", "numeric"),
            where = "BPCells"
        )
        storage.mode(y) <- "double"
        fn(x, y)
    }
)

#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "%*%", c(x = "numeric", y = "BPCellsSeed"), function(x, y) {
        fn <- methods::getMethod(
            "%*%", c("numeric", "IterableMatrix"),
            where = "BPCells"
        )
        storage.mode(x) <- "double"
        fn(x, y)
    }
)



#################### Matrix Crossproduct ########################
#' @importMethodsFrom DelayedArray crossprod
#' @return
#' - `crossprod(x, y)`: Matrix Crossproduct, a [BPCellsSeed] object or a dense
#'   matrix (matrix and numeric methods).
#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "crossprod", c(x = "BPCellsSeed", y = "BPCellsSeed"), function(x, y) {
        t(x) %*% y
    }
)

#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "crossprod", c(x = "BPCellsSeed", y = "dgCMatrix"), function(x, y) {
        t(x) %*% y
    }
)

#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "crossprod", c(x = "dgCMatrix", y = "BPCellsSeed"), function(x, y) {
        t(x) %*% y
    }
)

#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "crossprod", c(x = "BPCellsSeed", y = "ANY"), function(x, y) {
        t(x) %*% coerce_dgCMatrix(y)
    }
)

#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "crossprod", c(x = "ANY", y = "BPCellsSeed"), function(x, y) {
        t(coerce_dgCMatrix(x)) %*% y
    }
)

#################### Matrix Crossproduct ########################
# following methods return a dense matrix
#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "crossprod", c(x = "BPCellsSeed", y = "matrix"), function(x, y) {
        t(x) %*% y
    }
)

#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "crossprod", c(x = "matrix", y = "BPCellsSeed"), function(x, y) {
        t(x) %*% y
    }
)

#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "crossprod", c(x = "BPCellsSeed", y = "numeric"), function(x, y) {
        t(x) %*% y
    }
)

#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "crossprod", c(x = "numeric", y = "BPCellsSeed"), function(x, y) {
        t(x) %*% y
    }
)

#################### rbind ########################
#' @param threads Number of threads to use for execution. See [set_threads] for
#' details.
#' @importMethodsFrom methods rbind2
#' @export
#' @rdname BPCellsSeed-Class
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

#' @param use.first.dimnames Ignored, always be `TRUE` in BPCells.
#' @param deparse.level Ignored, used by generic methods.
#' @importMethodsFrom DelayedArray rbind
#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "rbind", "BPCellsSeed",
    function(..., threads = 0L, use.first.dimnames = TRUE, deparse.level = 1L) {
        merge_BPCellsSeeds(
            list = pack_BPCellsSeeds(...), .fn = rbind2, threads = threads
        )
    }
)

#' @importMethodsFrom DelayedArray arbind
#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "arbind", "BPCellsSeed",
    function(..., threads = 0L, use.first.dimnames = TRUE) {
        merge_BPCellsSeeds(
            list = pack_BPCellsSeeds(...), .fn = rbind2, threads = threads
        )
    }
)

#' @param use.names Ignored, always be `TRUE`.
#' @param ignore.mcols Ignored.
#' @param check Ignored.
#' @inheritParams S4Vectors::bindROWS
#' @importMethodsFrom DelayedArray bindROWS
#' @export
#' @rdname BPCellsSeed-Class
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
#' @importMethodsFrom methods cbind2
#' @export
#' @rdname BPCellsSeed-Class
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

#' @importMethodsFrom DelayedArray cbind
#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "cbind", "BPCellsSeed",
    function(..., threads = 0L, use.first.dimnames = TRUE, deparse.level = 1L) {
        merge_BPCellsSeeds(
            list = pack_BPCellsSeeds(...), .fn = cbind2, threads = threads
        )
    }
)

#' @importMethodsFrom DelayedArray acbind
#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod("acbind", "BPCellsSeed", function(..., threads = 0L, use.first.dimnames = TRUE) {
    merge_BPCellsSeeds(
        list = pack_BPCellsSeeds(...),
        .fn = cbind2, threads = threads
    )
})

#' @importMethodsFrom S4Vectors bindCOLS
#' @export
#' @rdname BPCellsSeed-Class
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
