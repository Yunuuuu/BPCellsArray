# Iterable_dgCMatrix_wrapper
methods::setClass("BPCellsdgCMatrixSeed",
    contains = c(
        "BPCellsBasicSeed",
        BPCells_class("Iterable_dgCMatrix_wrapper")
    )
)
summary.BPCellsdgCMatrixSeed <- function(object) {
    "Load dgCMatrix from memory"
}
methods::setMethod(
    "summary", "BPCellsdgCMatrixSeed",
    summary.BPCellsdgCMatrixSeed
)

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "Iterable_dgCMatrix_wrapper", function(x) {
    methods::as(x, "BPCellsdgCMatrixSeed")
})

############################################################
#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "dgCMatrix", function(x) {
    x <- methods::as(x, "IterableMatrix")
    methods::callGeneric()
})
