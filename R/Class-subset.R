###########################################################
# MatrixSubset-DelayedSubset
# used as a intermediate `BPCellsSeed` object of `subset_BPCellsSeed` function
# so we can re-dispath the seed contract methods of `BPCellsSeed`
mould_BPCells("BPCellsDelayedSubset", "MatrixSubset",
    rename = c(matrix = "seed"),
    contains = "BPCellsDelayedUnaryOp"
)

methods::setMethod("to_DelayedArray", "MatrixSubset", function(object) {
    to_DelayedUnaryOp(object, Class = "BPCellsDelayedSubset")
})

methods::setMethod("to_BPCells", "BPCellsDelayedSubset", function(object) {
    methods::callNextMethod(object = object, Class = "MatrixSubset")
})

# implement all methods of `DelayedSubset` such that `BPCellsDelayedSubset` will
# behaviour same with `DelayedSubset` object.
# https://github.com/Bioconductor/DelayedArray/blob/devel/R/DelayedSubset-class.R

#' @importFrom DelayedArray is_noop
methods::setMethod("is_noop", "BPCellsDelayedSubset", function(x) FALSE)

summary.BPCellsDelayedSubset <- function(object) {
    "Subset matrix"
}

methods::setMethod(
    "summary", "BPCellsDelayedSubset",
    summary.BPCellsDelayedSubset
)

################    BPCellsMatrix Methods    ##################
#' @inheritParams BPCellsSeed-class
#' @return
#' - `[`: A [BPCellsDelayedSubset] object.
#' @order 2
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod(
    "[", "BPCellsMatrix",
    set_BPCellsArray_method(
        x = , i = , j = , ... = , drop = TRUE,
        after = expression(
            ans <- DelayedArray(to_DelayedArray(object)),
            if (drop) drop(ans) else ans
        ),
        Arrays = "x"
    )
)
