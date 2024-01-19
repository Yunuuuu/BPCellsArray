#' Delayed MatrixDir arrays
#'
#' The `BPCellsDirArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `MatrixDir` object in
#' BPCells.
#'
#' @importClassesFrom BPCells MatrixDir
#' @name BPCellsDir
#' @seealso [writeBPCellsDirArray]
methods::setClass("BPCellsDirSeed", contains = "MatrixDir")

#' @param x For `BPCellsDirSeed` and `BPCellsDirArray`, a path of the
#' `MatrixDir` data, or a `MatrixDir` object. For other function, a
#' `BPCellsDirSeed` or `BPCellsDirArray` object.
#' @inheritParams BPCells::open_matrix_dir
#' @export
#' @rdname BPCellsDir
BPCellsDirSeed <- function(x, buffer_size = 8192L) {
    if (rlang::is_string(x)) {
        matrix_dir <- BPCells::open_matrix_dir(
            dir = x, buffer_size = buffer_size
        )
    } else if (methods::is(x, "MatrixDir")) {
        matrix_dir <- x
    } else {
        cli::cli_abort(
            "{.arg x} must be a {.cls string} or {.cls MatrixDir} object"
        )
    }
    methods::as(matrix_dir, "BPCellsDirSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @rdname BPCellsDir
methods::setClass("BPCellsDirArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsDirSeed"),
    prototype = list(seed = methods::new("BPCellsDirSeed"))
)

#' @param seed A `BPCellsDirSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @rdname BPCellsDir
methods::setMethod(
    "DelayedArray", "BPCellsDirSeed",
    function(seed) {
        new_DelayedArray(seed, Class = "BPCellsDirArray")
    }
)

#' @param ... Additional parameters passed into `BPCellsDirSeed`.
#' @export
#' @rdname BPCellsDir
BPCellsDirArray <- function(x, ...) {
    DelayedArray(BPCellsDirSeed(x, ...))
}

#' @export
#' @importClassesFrom DelayedArray DelayedMatrix
#' @rdname BPCellsDir
methods::setClass("BPCellsDirMatrix",
    contains = "DelayedMatrix",
    slots = c(seed = "BPCellsDirSeed")
)

#' @export
#' @importMethodsFrom DelayedArray matrixClass
#' @rdname BPCellsDir
methods::setMethod("matrixClass", "BPCellsDirArray", function(x) {
    "BPCellsDirMatrix"
})


###################################################################
###########################  Methods  #############################
###################################################################


#' Write a sparce matrices into a BPCells Directory of files format
#'
#' @param x Input matrix, any matrix can be coerced into
#' [dgCMatrix][Matrix::dgCMatrix-class] object.
#' @param path Directory to save the data into, if `NULL`, will use a temporary
#' directory.
#' @param ... Additional parameters passed to specific methods.
#' @inheritParams BPCells::write_matrix_dir
#' @return A [BPCellsDir] object.
#' @export
#' @name writeBPCellsDirArray
methods::setGeneric(
    "writeBPCellsDirArray",
    function(x, ...) standardGeneric("writeBPCellsDirArray")
)

.writeBPCellsDirArray <- function(
    x, path = NULL, compress = TRUE,
    buffer_size = 8192L,
    overwrite = FALSE) {
    path <- path %||% tempfile("BPCellsDirArray")
    obj <- BPCells::write_matrix_dir(
        mat = x, dir = path, compress = compress,
        buffer_size = buffer_size, overwrite = overwrite
    )
    BPCellsDirArray(obj)
}

#' @rdname writeBPCellsDirArray
methods::setMethod("writeBPCellsDirArray", "ANY", function(x, ...) {
    .writeBPCellsDirArray(x = methods::as(x, "dgCMatrix"), ...)
})

#' @rdname writeBPCellsDirArray
#' @importClassesFrom Matrix dgCMatrix
methods::setMethod("writeBPCellsDirArray", "dgCMatrix", .writeBPCellsDirArray)

#' @rdname writeBPCellsDirArray
#' @importClassesFrom BPCells IterableMatrix
methods::setMethod(
    "writeBPCellsDirArray", "IterableMatrix", .writeBPCellsDirArray
)

.as_BPCellsDirArray <- function(from) writeBPCellsDirArray(from)

#' @export
methods::setAs("ANY", "BPCellsDirArray", .as_BPCellsDirArray)

#' @export
methods::setAs("DelayedArray", "BPCellsDirArray", .as_BPCellsDirArray)

#' @export
methods::setAs("DelayedMatrix", "BPCellsDirMatrix", .as_BPCellsDirArray)

#' @param object A `BPCellsDirSeed` object.
#' @export
#' @importMethodsFrom methods show
#' @rdname BPCellsDir
methods::setMethod("show", "BPCellsDirSeed", function(object) {
    cat(sprintf("%i x %i BPCellsDirSeed object\n", nrow(object), ncol(object)))
})

#' @export
#' @importMethodsFrom DelayedArray path
#' @rdname BPCellsDir
methods::setMethod("path", "BPCellsDirSeed", function(object) object@dir)

#' @export
#' @importMethodsFrom DelayedArray type
#' @include utils.R
#' @rdname BPCellsDir
methods::setMethod("type", "BPCellsDirSeed", bpcells_to_r_type)

#' @export
#' @importMethodsFrom DelayedArray is_sparse
#' @rdname BPCellsDir
methods::setMethod("is_sparse", "BPCellsDirSeed", function(x) TRUE)

#' @inheritParams S4Arrays::extract_array
#' @export
#' @importMethodsFrom DelayedArray extract_array
#' @rdname BPCellsDir
methods::setMethod(
    "extract_array", "BPCellsDirSeed",
    function(x, index) {
        out <- as.matrix(extract_bpcells_array(x, index))
        storage.mode(out) <- type(x)
        out
    }
)

#' @export
#' @importMethodsFrom DelayedArray extract_sparse_array
#' @rdname BPCellsDir
methods::setMethod(
    "extract_sparse_array", "BPCellsDirSeed",
    function(x, index) {
        methods::as(extract_bpcells_array(x, index), "SparseArraySeed")
    }
)

#' @param i,j Row and Column index.
#' @param drop Not used, always be `FALSE`.
#' @export
#' @rdname BPCellsDir
methods::setMethod(
    "[", "BPCellsDirSeed",
    function(x, i, j, ..., drop = FALSE) {
        BPCellsMatrixSubsetSeed(x[i, j])
    }
)

#' @export
#' @rdname BPCellsDir
methods::setMethod(
    "[", "BPCellsDirArray",
    function(x, i, j, ..., drop = FALSE) {
        BPCellsMatrixSubsetArray(x@seed[i, j])
    }
)
