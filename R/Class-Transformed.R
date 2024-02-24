mould_BPCells("BPCellsDelayedTransformed", "TransformedMatrix",
    delete = "matrix",
    contains = "BPCellsDelayedUnaryIsoOp"
)

methods::setGeneric("DelayedTransformedClass", function(x) {
    standardGeneric("DelayedTransformedClass")
})

methods::setMethod("to_DelayedArray", "TransformedMatrix", function(object) {
    to_DelayedUnaryOp(object, Class = DelayedTransformedClass(object))
})

methods::setGeneric("BPCellsTransformedClass", function(x) {
    standardGeneric("BPCellsTransformedClass")
})

methods::setMethod("to_BPCells", "BPCellsDelayedTransformed", function(object) {
    methods::callNextMethod(
        object = object,
        Class = BPCellsTransformedClass(object)
    )
})

#########################################################
summary.BPCellsDelayedTransformed <- function(object) {
    cls <- gsub("^BPCellsDelayedTransforme", "", class(object)[1L])
    sprintf(
        "Transform by %s",
        switch(cls,
            ScaleShift = "scale and (or) shift",
            Expm1Slow = "expm1_slow",
            Min = "pmin_scalar",
            MinByCol = ,
            MinByRow = paste0("p", snakeize(cls)),
            snakeize(cls)
        )
    )
}

methods::setMethod(
    "summary", "BPCellsDelayedTransformed",
    summary.BPCellsDelayedTransformed
)
