###########################################################
# MatrixSubset
#' @importClassesFrom DelayedArray DelayedSubset
mould_BPCells("BPCellsDelayedSubset", "MatrixSubset",
    remove = c("matrix", "row_selection", "col_selection"),
    # BPCellsDelayedUnaryOp: `seed` slot
    # DelayedSubset: `index` slot
    contains = c("DelayedSubset", "BPCellsDelayedUnaryOp")
)

### list_methods("DelayedSubset")
### Seed contract
### here: we override the `DelayedSubset` methods
#' @export
#' @rdname BPCellsDelayedOp-class
methods::setMethod(
    "dim", "BPCellsDelayedSubset",
    delayedop_call_BPCells_method(x = )
)

#' @export
#' @rdname BPCellsDelayedOp-class
methods::setMethod(
    "dimnames", "BPCellsDelayedSubset",
    delayedop_call_BPCells_method(x = )
)

#' @export
#' @rdname BPCellsDelayedOp-class
methods::setMethod("is_sparse", "BPCellsDelayedSubset", function(x) TRUE)
#' @export
#' @rdname BPCellsDelayedOp-class
methods::setMethod(
    "extract_array", "BPCellsDelayedSubset",
    delayedop_call_BPCells_method(x = , index = )
)

#' @export
#' @rdname BPCellsDelayedOp-class
methods::setMethod(
    "OLD_extract_sparse_array", "BPCellsDelayedSubset",
    delayedop_call_BPCells_method(x = , index = )
)

#' @importFrom DelayedArray is_noop
#' @export
#' @rdname BPCellsDelayedOp-class
methods::setMethod("is_noop", "BPCellsDelayedSubset", function(x) FALSE)

#' @export
#' @rdname BPCellsDelayedOp-class
methods::setMethod(
    "chunkdim", "BPCellsDelayedSubset",
    delayedop_call_BPCells_method(x = )
)

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

summary.BPCellsDelayedSubset <- function(object) {
    "Subset matrix"
}

methods::setMethod(
    "summary", "BPCellsDelayedSubset",
    summary.BPCellsDelayedSubset
)
summary.MatrixSubset <- summary.BPCellsDelayedSubset
methods::setMethod("summary", "MatrixSubset", summary.BPCellsDelayedSubset)


################    BPCellsMatrix Methods    ##################
#' @param i,j Row and Column index.
#' @param ... Not used currently.
#' @return
#' - `[`: A `BPCellsMatrix` object or an atomic vector.
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod(
    "[", "BPCellsMatrix",
    array_call_BPCells_method(
        x = , i = , j = , ... = , drop = TRUE,
        body = expression(if (drop) drop(object) else object)
    )
)