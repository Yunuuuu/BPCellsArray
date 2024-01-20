#' Low-level Base Class for Delayed BPCells matrix
#'
#' The `BPCellsSeed` class just inherits from the `IterableMatrix` object in
#' BPCells package. The purpose for `BPCellsSeed` object is to provide the
#' common methods for all low-level BPCells seed objects.
#'
#' @param x A [BPCellsSeed] object. For some functions:
#'  - `BPCellsSeed`: a [BPCellsSeed] object, or other BPCells matrix object.
#'  - `matrix_multi`, `%*%`, and `crossprod`: A [BPCellsSeed] object or
#'     matrx-like object which can be coerced into a
#'     [dgCMatrix][Matrix::dgCMatrix-class].
#' @param y A [BPCellsSeed] object or matrx-like object which can be coerced
#'   into a [dgCMatrix][Matrix::dgCMatrix-class].
#' @param ... Additional arguments passed to specific methods.
#'      For some functions:
#'  - `matrix_multi`: other arguments passed to
#'    [transpose_storage_order][BPCells::transpose_storage_order].
#' @importClassesFrom BPCells IterableMatrix
#' @export
#' @name BPCellsSeed
methods::setClass("BPCellsSeed", contains = "IterableMatrix")

#' @export
#' @rdname BPCellsSeed
methods::setGeneric("BPCellsSeed", function(x, ...) {
    standardGeneric("BPCellsSeed")
})

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "BPCellsSeed", function(x, ...) {
    rlang::check_dots_empty()
    x
})

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "MatrixSubset", function(x, ...) {
    rlang::check_dots_empty()
    BPCellsSubsetSeed(x = x)
})

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "ConvertMatrixType", function(x, ...) {
    rlang::check_dots_empty()
    BPCellsConvertSeed(x = x)
})

#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "MatrixMultiply", function(x, ...) {
    rlang::check_dots_empty()
    BPCellsMultiplySeed(x = x)
})

#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "RenameDims", function(x, ...) {
    rlang::check_dots_empty()
    BPCellsRenameDimsSeed(x = x)
})

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "ColBindMatrices", function(x, ...) {
    rlang::check_dots_empty()
    BPCellsColBindMatrixSeed(x = x)
})

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "RowBindMatrices", function(x, ...) {
    rlang::check_dots_empty()
    BPCellsRowBindMatrixSeed(x = x)
})

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "MatrixDir", function(x, ...) {
    BPCellsDirSeed(x = x, ...)
})

#' @param object A `BPCellsSeed` object.
#' @importMethodsFrom methods show
#' @export
#' @rdname BPCellsSeed
methods::setMethod("show", "BPCellsSeed", function(object) {
    show_bpcells(object, "BPCellsSeed", class(object))
})

#' @importMethodsFrom DelayedArray type
#' @export
#' @rdname BPCellsSeed
#' @include utils.R
methods::setMethod("type", "BPCellsSeed", function(x) {
    switch(BPCells:::matrix_type(x),
        uint32_t = "integer",
        float = "double",
        double = "double"
    )
})

#' @importMethodsFrom DelayedArray is_sparse
#' @export
#' @rdname BPCellsSeed
methods::setMethod("is_sparse", "BPCellsSeed", function(x) TRUE)

#' @inheritParams S4Arrays::extract_array
#' @importMethodsFrom DelayedArray extract_array
#' @export
#' @rdname BPCellsSeed
methods::setMethod(
    "extract_array", "BPCellsSeed",
    function(x, index) {
        out <- as.matrix(extract_bpcells_array(x, index))
        storage.mode(out) <- type(x)
        out
    }
)

#' @importMethodsFrom DelayedArray extract_sparse_array
#' @export
#' @rdname BPCellsSeed
methods::setMethod(
    "extract_sparse_array", "BPCellsSeed",
    function(x, index) {
        methods::as(extract_bpcells_array(x, index), "SparseArraySeed")
    }
)

methods::setClassUnion("ListOrNULL", c("list", "NULL"))
# In BPCells, `dimnames<-` was only defined for IterableMatrix
methods::setMethod(
    "dimnames<-",
    signature(x = "BPCellsSeed", value = "ListOrNULL"), function(x, value) {
        BPCellsSeed(methods::callNextMethod())
    }
)

# t will not change the underlying class
#' @importMethodsFrom DelayedArray t
#' @export
#' @rdname BPCellsSeed
methods::setMethod("t", "BPCellsSeed", function(x) methods::callNextMethod())

# In BPCells, `[<-` was only defined for IterableMatrix
#' @param i,j Row and Column index.
#' @importMethodsFrom BPCells [<-
#' @export
#' @rdname BPCellsSeed
methods::setMethod(
    "[<-", "BPCellsSeed", function(x, i, j, ..., value) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#################### Matrix multiplication ########################
#' @export
#' @rdname BPCellsSeed
methods::setGeneric("matrix_multi", function(x, y, ...) {
    standardGeneric("matrix_multi")
})


#' @return
#' - `matrix_multi`: matrix multiplication, a [BPCellsSeed] object
#' @export
#' @rdname BPCellsSeed
methods::setMethod(
    "matrix_multi",
    signature(x = "BPCellsSeed", y = "BPCellsSeed"), function(x, y, ...) {
        if (x@transpose != y@transpose) {
            if (x@transpose) {
                cli::cli_inform("transpose storage order for {.arg x}")
                x <- BPCells::transpose_storage_order(matrix = x, ...)
            } else {
                cli::cli_inform("transpose storage order for {.arg y}")
                y <- BPCells::transpose_storage_order(matrix = y, ...)
            }
        }
        BPCellsSeed(methods::callNextMethod(x, y))
    }
)

#' @export
#' @rdname BPCellsSeed
methods::setMethod(
    "matrix_multi", signature(x = "BPCellsSeed", y = "dgCMatrix"),
    function(x, y) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @export
#' @rdname BPCellsSeed
methods::setMethod(
    "matrix_multi", signature(x = "dgCMatrix", y = "BPCellsSeed"),
    function(x, y) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @export
#' @rdname BPCellsSeed
methods::setMethod(
    "matrix_multi", signature(x = "BPCellsSeed", y = "ANY"), function(x, y) {
        x %*% coerce_dgCMatrix(y)
    }
)

#' @export
#' @rdname BPCellsSeed
methods::setMethod(
    "matrix_multi", signature(x = "ANY", y = "BPCellsSeed"), function(x, y) {
        coerce_dgCMatrix(x) %*% y
    }
)

###################### Matrix multiplication ########################
#' @importMethodsFrom DelayedArray %*%
#' @return
#' - `x %*% y`: matrix multiplication, a [BPCellsSeed] object
#' @export
#' @rdname BPCellsSeed
methods::setMethod(
    "%*%", signature(x = "BPCellsSeed", y = "BPCellsSeed"), function(x, y) {
        matrix_multi(x, y)
    }
)

#' @export
#' @rdname BPCellsSeed
methods::setMethod(
    "%*%", signature(x = "BPCellsSeed", y = "dgCMatrix"), function(x, y) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @export
#' @rdname BPCellsSeed
methods::setMethod(
    "%*%", signature(x = "dgCMatrix", y = "BPCellsSeed"), function(x, y) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @export
#' @rdname BPCellsSeed
methods::setMethod(
    "%*%", signature(x = "BPCellsSeed", y = "ANY"), function(x, y) {
        x %*% coerce_dgCMatrix(y)
    }
)

#' @export
#' @rdname BPCellsSeed
methods::setMethod(
    "%*%", signature(x = "ANY", y = "BPCellsSeed"), function(x, y) {
        coerce_dgCMatrix(x) %*% y
    }
)

#################### Matrix Crossproduct ########################
#' @importMethodsFrom DelayedArray crossprod
#' @return
#' - `crossprod(x, y)`: Matrix Crossproduct, a [BPCellsSeed] object
#' @export
#' @rdname BPCellsSeed
methods::setMethod(
    "crossprod",
    signature(x = "BPCellsSeed", y = "BPCellsSeed"), function(x, y) {
        t(x) %*% y
    }
)

#' @export
#' @rdname BPCellsSeed
methods::setMethod(
    "crossprod",
    signature(x = "BPCellsSeed", y = "dgCMatrix"), function(x, y) {
        t(x) %*% y
    }
)

#' @export
#' @rdname BPCellsSeed
methods::setMethod(
    "crossprod",
    signature(x = "dgCMatrix", y = "BPCellsSeed"), function(x, y) {
        t(x) %*% y
    }
)

#' @export
#' @rdname BPCellsSeed
methods::setMethod(
    "crossprod",
    signature(x = "BPCellsSeed", y = "matrix"), function(x, y) {
        t(x) %*% y
    }
)

#' @export
#' @rdname BPCellsSeed
methods::setMethod(
    "crossprod",
    signature(x = "matrix", y = "BPCellsSeed"), function(x, y) {
        t(x) %*% y
    }
)

#' @export
#' @rdname BPCellsSeed
methods::setMethod(
    "crossprod",
    signature(x = "BPCellsSeed", y = "ANY"), function(x, y) {
        t(x) %*% coerce_dgCMatrix(y)
    }
)

#' @export
#' @rdname BPCellsSeed
methods::setMethod(
    "crossprod", signature(x = "ANY", y = "BPCellsSeed"), function(x, y) {
        t(coerce_dgCMatrix(x)) %*% y
    }
)
