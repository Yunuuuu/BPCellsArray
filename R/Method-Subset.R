###########################################################
# MatrixSubset
#' @importClassesFrom DelayedArray DelayedSubset
mould_BPCells("BPCellsDelayedSubset", "MatrixSubset",
    remove = c("matrix", "row_selection", "col_selection"),
    # DelayedSubset: `seed` and `index` slot
    contains = c("BPCellsDelayedOp", "DelayedSubset")
)
#' @importFrom DelayedArray is_noop
#' @export
#' @rdname BPCellsDelayedOp-class
methods::setMethod("is_noop", "BPCellsDelayedSubset", function(x) FALSE)

#############################################################
methods::setMethod("to_DelayedArray", "MatrixSubset", function(object) {
    # https://github.com/bnprks/BPCells/blob/919983d1c2237555c95f5a140f37be309116883b/R/matrix.R#L597
    index <- list(object@row_selection, object@col_selection)
    # BPCells use `rlang::missing_arg()` directly
    # but DelayedArray use `NULL`
    index <- lapply(index, function(x) {
        if (rlang::is_missing(x)) NULL else x
    })
    object <- migrate_slots(
        Object = object,
        rename = c(matrix = "seed"),
        remove = c("row_selection", "col_selection"),
        Class = "BPCellsDelayedSubset"
    )
    object@seed <- to_DelayedArray(object@seed)
    object@index <- index
    object
})

methods::setMethod("to_BPCells", "BPCellsDelayedSubset", function(object) {
    index <- object@index
    names(index) <- c("row_selection", "col_selection")
    index <- lapply(index, function(x) {
        if (is.null(x)) rlang::missing_arg() else x
    })
    object@seed <- to_BPCells(object@seed)
    migrate_slots(
        Object = object,
        rename = c(seed = "matrix"),
        remove = "index",
        new = index,
        Class = "MatrixSubset"
    )
})

#' @exportS3Method base::summary
summary.BPCellsDelayedSubset <- function(object) {
    "Subset matrix"
}

methods::setMethod(
    "summary", "BPCellsDelayedSubset",
    summary.BPCellsDelayedSubset
)

#' @exportS3Method base::summary
summary.MatrixSubset <- summary.BPCellsDelayedSubset
methods::setMethod("summary", "MatrixSubset", summary.BPCellsDelayedSubset)


################    BPCellsMatrix Methods    ##################
#' @param i,j Row and Column index.
#' @return
#' - `[`: A `BPCellsMatrix` object or an atomic vector.
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("[", "BPCellsMatrix", function(x, i, j, drop = TRUE) {
    x <- to_BPCells(x@seed)
    ans <- methods::callGeneric()
    ans <- DelayedArray(ans)
    if (drop) {
        drop(ans)
    } else {
        ans
    }
})
