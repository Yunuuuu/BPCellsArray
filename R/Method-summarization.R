#' BPCellsMatrix row/col summarization
#'
#' @param x A [BPCellsMatrix][BPCellsMatrix-class] or
#' [BPcellsSeed][BPCellsSeed-class]object. 
#' @name BPCells-Summarization
NULL

########################################################################
# matrix_stats
#' @inherit BPCells::matrix_stats
#' @aliases matrix_stats
#' @return 
#' - `matrix_stats`: A list of 
#'    - `row_stats`: matrix of n_stats x n_rows
#'    - `col_stats`: matrix of n_stats x n_cols
#' @param object A [BPCellsMatrix] object.
#' @export
#' @rdname BPCells-Summarization
methods::setGeneric("matrix_stats", function(object, ...) {
    makeStandardGeneric("matrix_stats")
})

#' @export
#' @rdname BPCells-Summarization
methods::setMethod(
    "matrix_stats", "BPCellsSeed", function(object, ...) {
        BPCells::matrix_stats(matrix = object, ...)
    }
)

#' @inheritDotParams BPCells::matrix_stats -matrix
#' @export
#' @rdname BPCells-Summarization
methods::setMethod(
    "matrix_stats", "BPCellsMatrix", function(object, ...) {
        BPCells::matrix_stats(matrix = object@seed, ...)
    }
)

#######################################################################
# Sums
#' @importMethodsFrom BPCells rowSums
#' @return
#' - `rowSums()`: vector of row sums
#' @aliases rowSums
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("rowSums", c(x = "BPCellsMatrix"), function(x) {
    rowSums(x@seed)
})

#' @importMethodsFrom BPCells colSums
#' @return
#' - `colSums()`: vector of column sums
#' @aliases colSums
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("colSums", c(x = "BPCellsMatrix"), function(x) {
    colSums(x@seed)
})

#######################################################################
# Means
#' @importMethodsFrom BPCells rowMeans
#' @return
#' - `rowMeans()`: vector of row means
#' @aliases rowMeans
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("rowMeans", c(x = "BPCellsMatrix"), function(x) {
    rowMeans(x@seed)
})

#' @importMethodsFrom BPCells colMeans
#' @return
#' - `colMeans()`: vector of column means
#' @aliases colMeans
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("colMeans", c(x = "BPCellsMatrix"), function(x) {
    colMeans(x@seed)
})

#######################################################################
# Variance
#' @importFrom MatrixGenerics rowVars
#' @return
#' - `rowVars()`: vector of row variance
#' @aliases rowVars
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("rowVars", c(x = "BPCellsSeed"), function(x) {
    stats <- BPCells::matrix_stats(x,
        row_stats = "variance", col_stats = "none"
    )
    stats$row_stats["variance", , drop = TRUE]
})

#' @importFrom MatrixGenerics colVars
#' @return
#' - `colVars()`: vector of column variance
#' @aliases colVars
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("colVars", c(x = "BPCellsSeed"), function(x) {
    stats <- BPCells::matrix_stats(x,
        row_stats = "none", col_stats = "variance"
    )
    stats$col_stats["variance", , drop = TRUE]
})

#' @importFrom MatrixGenerics rowVars
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("rowVars", c(x = "BPCellsMatrix"), function(x) {
    rowVars(x@seed)
})

#' @importFrom MatrixGenerics colVars
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("colVars", c(x = "BPCellsMatrix"), function(x) {
    colVars(x@seed)
})

#######################################################################
# Standard Deviation
#' @importFrom MatrixGenerics rowSds
#' @return
#' - `rowSds()`: vector of row Standard Deviation
#' @aliases rowSds
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("rowSds", c(x = "BPCellsSeed"), function(x) {
    sqrt(rowVars(x))
})

#' @importFrom MatrixGenerics colSds
#' @return
#' - `colSds()`: vector of column Standard Deviation
#' @aliases colSds
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("colSds", c(x = "BPCellsSeed"), function(x) {
    sqrt(colVars(x))
})

#' @importFrom MatrixGenerics rowSds
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("rowSds", c(x = "BPCellsMatrix"), function(x) {
    rowSds(x@seed)
})

#' @importFrom MatrixGenerics colSds
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("colSds", c(x = "BPCellsMatrix"), function(x) {
    colSds(x@seed)
})
