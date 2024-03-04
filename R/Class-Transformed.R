mould_BPCells("BPCellsDelayedTransformed", "TransformedMatrix",
    remove = "matrix",
    # BPCellsDelayedUnaryIsoOp: `seed` slot
    contains = "BPCellsDelayedUnaryIsoOp"
)

methods::setMethod("to_DelayedArray", "TransformedMatrix", function(object) {
    to_DelayedUnaryOp(object,
        Class = paste0("BPCellsDelayed", fclass(object))
    )
})

methods::setMethod("to_BPCells", "BPCellsDelayedTransformed", function(object) {
    to_BPCellsUnaryOp(
        object = object,
        Class = sub("^BPCellsDelayed", "", fclass(object))
    )
})

#########################################################
summary.BPCellsDelayedTransformed <- function(object) {
    cls <- sub("^(BPCellsDelayed)?Transform", "", fclass(object))
    sprintf(
        "Transform by %s",
        switch(cls,
            Log1p = "`log1p` (single-precision)",
            Log1pSlow = "`log1p` (double-precision)",
            ScaleShift = "scale and (or) shift",
            Expm1Slow = "`expm1` (slow version)",
            Square = ,
            Pow = "`^`",
            PowSlow = "`^` (slow version)",
            Min = "`pmin_scalar`",
            MinByCol = ,
            MinByRow = paste0("`p", snakeize(cls), "`"),
            sprintf("`%s`", snakeize(cls))
        )
    )
}

methods::setMethod(
    "summary", "BPCellsDelayedTransformed",
    summary.BPCellsDelayedTransformed
)
summary.TransformedMatrix <- summary.BPCellsDelayedTransformed
methods::setMethod(
    "summary", "TransformedMatrix",
    summary.BPCellsDelayedTransformed
)
