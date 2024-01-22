#' @inherit BPCells::matrix_stats
#' @aliases matrix_stats
#' @return A [BPCellsMatrix][BPCellsMatrix-class] object.
#' @name BPCells-matrix_stats
NULL

#' @param object A [BPCellsMatrix] object.
#' @export
#' @rdname BPCells-matrix_stats
methods::setGeneric("matrix_stats", function(object, ...) {
    makeStandardGeneric("matrix_stats")
})

#' @export
#' @rdname seed-methods
methods::setMethod(
    "matrix_stats", "BPCellsSeed", function(object, ...) {
        BPCells::matrix_stats(matrix = object, ...)
    }
)

#' @inheritDotParams BPCells::matrix_stats -matrix
#' @export
#' @rdname BPCells-matrix_stats
methods::setMethod(
    "matrix_stats", "BPCellsMatrix", function(object, ...) {
        BPCells::matrix_stats(matrix = object@seed, ...)
    }
)
