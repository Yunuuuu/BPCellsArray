#' @importMethodsFrom DelayedArray path
#' @export
#' @rdname seed-methods
methods::setMethod("path", "BPCellsColBindMatrixSeed", function(object) {
    unlist(lapply(object@matrix_list, path),
        recursive = FALSE, use.names = FALSE
    )
})

#' @importMethodsFrom DelayedArray path
#' @export
#' @rdname seed-methods
methods::setMethod("path", "BPCellsRowBindMatrixSeed", function(object) {
    unlist(lapply(object@matrix_list, path),
        recursive = FALSE, use.names = FALSE
    )
})

#' @importMethodsFrom DelayedArray path
#' @export
#' @rdname seed-methods
methods::setMethod("path", "BPCellsConvertSeed", function(object) {
    path(object@matrix)
})

#' @importMethodsFrom DelayedArray path
#' @export
#' @rdname seed-methods
methods::setMethod("path", "BPCellsdgCMatrixSeed", function(object) {
    NULL
})

#' @importMethodsFrom DelayedArray path
#' @export
#' @rdname seed-methods
methods::setMethod("path", "BPCellsDirSeed", function(object) object@dir)

#' @importMethodsFrom DelayedArray path
#' @export
#' @rdname seed-methods
methods::setMethod("path", "BPCellsMaskSeed", function(object) {
    c(path(object@matrix), path(object@mask))
})

#' @importMethodsFrom DelayedArray path
#' @export
#' @rdname seed-methods
methods::setMethod("path", "BPCellsMultiplySeed", function(object) {
    c(path(object@left), path(object@right))
})

#' @importMethodsFrom DelayedArray path
#' @export
#' @rdname seed-methods
methods::setMethod("path", "BPCellsRankTransformSeed", function(object) {
    path(object@matrix)
})

#' @importMethodsFrom DelayedArray path
#' @export
#' @rdname seed-methods
methods::setMethod("path", "BPCellsRenameDimsSeed", function(object) {
    path(object@matrix)
})

#' @importMethodsFrom DelayedArray path
#' @export
#' @rdname seed-methods
methods::setMethod("path", "BPCellsSubsetSeed", function(object) {
    path(object@matrix)
})

#' @importMethodsFrom DelayedArray path
#' @export
#' @rdname seed-methods
methods::setMethod("path", "BPCellsTransformedSeed", function(object) {
    path(object@matrix)
})
