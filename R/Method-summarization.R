#' BPCellsMatrix row/col summarization
#'
#' @param x A [BPCellsMatrix][BPCellsMatrix] object.
#' @return
#' - `rowSums()`: vector of row sums
#' @aliases rowSums
#' @name BPCells-Summarization
NULL

#' @importMethodsFrom BPCells rowSums
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("rowSums", c(x = "BPCellsMatrix"), function(x) {
    rowSums(x@seed)
})

#' @importMethodsFrom BPCells colSums
#' @return
#' - `colSums()`: vector of col sums
#' @aliases colSums
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("colSums", c(x = "BPCellsMatrix"), function(x) {
    colSums(x@seed)
})

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
#' - `colMeans()`: vector of col means
#' @aliases colMeans
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("colMeans", c(x = "BPCellsMatrix"), function(x) {
    colMeans(x@seed)
})

#' @importFrom DelayedArray rowVars
#' @return
#' - `rowVars()`: vector of row vars
#' @aliases rowVars
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("rowVars", c(x = "BPCellsMatrix"), function(x) {
    stats <- BPCells::matrix_stats(x@seed,
        row_stats = "variance", col_stats = "none"
    )
    stats$row_stats["variance", , drop = TRUE]
})

#' @importFrom DelayedArray colVars
#' @return
#' - `colVars()`: vector of col vars
#' @aliases colVars
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("colVars", c(x = "BPCellsMatrix"), function(x) {
    stats <- BPCells::matrix_stats(x@seed,
        row_stats = "none", col_stats = "variance"
    )
    stats$col_stats["variance", , drop = TRUE]
})
