#' BPCellsMatrix row/col summarization
#'
#' @name BPCells-Summarization
NULL

########################################################################
# matrix_stats
#' @inherit BPCells::matrix_stats
#' @aliases matrix_stats
#' @return
#' - `matrix_stats`: A list of
#'    - `row_stats`: matrix of `n_stats` x `n_rows`
#'    - `col_stats`: matrix of `n_stats` x `n_cols`
#' @inheritParams convert_mode
#' @export
#' @rdname BPCells-Summarization
methods::setGeneric("matrix_stats", function(object, ...) {
    standardGeneric("matrix_stats")
})

#' @inheritDotParams BPCells::matrix_stats -matrix
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("matrix_stats", "BPCellsMatrix", function(object, ...) {
    BPCells::matrix_stats(matrix = to_BPCells(object@seed), ...)
})

#######################################################################
# Sums
#' @inheritParams BPCells-Math
#' @importMethodsFrom BPCells rowSums
#' @return
#' - `rowSums()`: vector of row sums
#' @aliases rowSums
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("rowSums", c(x = "BPCellsMatrix"), function(x) {
    rowSums(to_BPCells(x@seed))
})

#' @importMethodsFrom BPCells colSums
#' @return
#' - `colSums()`: vector of column sums
#' @aliases colSums
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("colSums", c(x = "BPCellsMatrix"), function(x) {
    colSums(to_BPCells(x@seed))
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
    rowMeans(to_BPCells(x@seed))
})

#' @importMethodsFrom BPCells colMeans
#' @return
#' - `colMeans()`: vector of column means
#' @aliases colMeans
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("colMeans", c(x = "BPCellsMatrix"), function(x) {
    colMeans(to_BPCells(x@seed))
})

#######################################################################
# Variance
#' @importFrom MatrixGenerics rowVars
#' @return
#' - `rowVars()`: vector of row variance
#' @aliases rowVars
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("rowVars", c(x = "BPCellsMatrix"), function(x) {
    stats <- BPCells::matrix_stats(
        to_BPCells(x@seed),
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
methods::setMethod("colVars", c(x = "BPCellsMatrix"), function(x) {
    stats <- BPCells::matrix_stats(to_BPCells(x@seed),
        row_stats = "none", col_stats = "variance"
    )
    stats$col_stats["variance", , drop = TRUE]
})

#######################################################################
# Standard Deviation
#' @importFrom MatrixGenerics rowSds
#' @return
#' - `rowSds()`: vector of row Standard Deviation
#' @aliases rowSds
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("rowSds", c(x = "BPCellsMatrix"), function(x) {
    sqrt(rowVars(x))
})

#' @importFrom MatrixGenerics colSds
#' @return
#' - `colSds()`: vector of column Standard Deviation
#' @aliases colSds
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("colSds", c(x = "BPCellsMatrix"), function(x) {
    sqrt(colVars(x))
})


#######################################################################
# Max value
#' @importFrom MatrixGenerics rowMaxs
#' @return
#' - `rowMaxs()`: vector of row max values
#' @aliases rowMaxs
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("rowMaxs", c(x = "BPCellsMatrix"), function(x) {
    BPCells::rowMaxs(to_BPCells(x@seed))
})

#' @importFrom MatrixGenerics colMaxs
#' @return
#' - `colMaxs()`: vector of column max values
#' @aliases colMaxs
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("colMaxs", c(x = "BPCellsMatrix"), function(x) {
    BPCells::colMaxs(to_BPCells(x@seed))
})
