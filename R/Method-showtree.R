#' Visualize and access the leaves of a tree of delayed operations
#'
#' `showtree` can be used to visualize the tree of delayed operations carried by
#' a `BPCellsArray` object.
#' Use `nseed`, `seed`, or `path` to access the number of seeds, the seed, or
#' the seed path of a [BPCellsSeed][BPCellsSeed-class] or
#' [BPCellsMatrix][BPCellsMatrix-class] object, respectively.
#'
#' @param x,object A [BPCellsSeed][BPCellsSeed-class] or
#' [BPCellsMatrix][BPCellsMatrix-class] object
#' @return
#'  - `showtree`: return the input invisiblely
#'  - `path`: An atomic characters
#'  - `nseed`: An integer number
#'  - `seed`: A list of [BPCellsSeed][BPCellsSeed-class] objects or a single
#'    [BPCellsSeed][BPCellsSeed-class] object
#' @name showtree
showtree <- function(object) {
    if (methods::is(object, "BPCellsMatrix")) {
        object <- object@seed
    } else if (!methods::is(object, "BPCellsSeed")) {
        cli::cli_abort("{.arg object} must be a {.cls BPCellsSeed} or {.cls BPCellsMatrix} object")
    }
    .rec_showtree(x = object)
    invisible(object)
}

### Avoid use of non-ASCII characters in R source code. There must be a
### better way to do this.
.VBAR <- rawToChar(as.raw(c(0xe2, 0x94, 0x82)))
.TEE <- rawToChar(as.raw(c(0xe2, 0x94, 0x9c)))
.ELBOW <- rawToChar(as.raw(c(0xe2, 0x94, 0x94)))
.HBAR <- rawToChar(as.raw(c(0xe2, 0x94, 0x80)))

### 'last.child' can be NA, TRUE, or FALSE. NA means 'x' is the root of the
### tree.
.rec_showtree <- function(x, indent = "", last.child = NA, prefix = "") {
    if (methods::is(x, "BPCellsSeed")) {
        ## Display summary line.
        if (is.na(last.child)) {
            ## No Tprefix.
            Tprefix <- ""
        } else {
            ## 3-char Tprefix
            Tprefix <- paste0(if (last.child) .ELBOW else .TEE, .HBAR, " ")
        }
        x_as1line <- utils::tail(BPCells:::short_description(x), n = 1L)
        cat(indent, Tprefix, prefix, x_as1line, "\n", sep = "")
    }
    ## Display children.
    if (!is.na(last.child)) {
        ## Increase indent by 3 chars.
        indent <- paste0(
            indent,
            if (last.child) " " else .VBAR,
            strrep(" ", 2 + nchar(prefix))
        )
    }
    if (methods::is(x, "BPCellsUnaryOpsSeed")) {
        Recall(x = seed(x), indent = indent, last.child = TRUE)
    } else if (methods::is(x, "BPCellsNaryOpsSeed")) {
        x <- seed(x)
        nchildren <- length(x)
        nms <- names(x)
        if (is.null(nms)) {
            nms <- rep_len("", nchildren)
        } else {
            nms <- sprintf("%s: ", nms)
        }
        for (i in seq_len(nchildren)) {
            Recall(
                x = x[[i]], indent = indent,
                last.child = (i == nchildren),
                prefix = nms[[i]]
            )
        }
    }
}

#################################################################
#' @importFrom DelayedArray nseed
#' @export
#' @rdname showtree
methods::setMethod("nseed", "BPCellsMatrix", function(x) {
    nseed(x@seed)
})

#' @importFrom DelayedArray seed
#' @export
#' @rdname showtree
methods::setMethod("seed", "BPCellsMatrix", function(x) {
    seed(x@seed)
})

#' @importFrom DelayedArray path
#' @export
#' @rdname showtree
methods::setMethod("path", "BPCellsMatrix", function(object) {
    path(object@seed)
})

#' @importFrom DelayedArray seed<-
#' @export
#' @rdname internal-methods
methods::setReplaceMethod("seed", "BPCellsMatrix", function(x, value) {
    cli::cli_abort("Cannot set {.field seed} for {.cls BPCellsMatrix} object.")
})

#' @importFrom DelayedArray path<-
#' @export
#' @rdname internal-methods
methods::setReplaceMethod("path", "BPCellsMatrix", function(object, value) {
    cli::cli_abort("Cannot set {.field path} for {.cls BPCellsMatrix} object.")
})

# Three DelayedNaryOp-like seeds ----------------------------------------
###########################################################
#' @importFrom DelayedArray path
#' @export
#' @rdname showtree
methods::setMethod("path", "BPCellsBindMatrixSeed", function(object) {
    unlist(lapply(object@matrix_list, path),
        recursive = FALSE, use.names = FALSE
    )
})

#' @importFrom DelayedArray seed
#' @export
#' @rdname showtree
methods::setMethod("seed", "BPCellsBindMatrixSeed", function(x) {
    lapply(x@matrix_list, seed)
})

#' @export
#' @rdname showtree
methods::setMethod("nseed", "BPCellsBindMatrixSeed", function(x) {
    sum(vapply(x@matrix_list, nseed, integer(1L), USE.NAMES = FALSE))
})

###########################################################
#' @importFrom DelayedArray path
#' @export
#' @rdname showtree
methods::setMethod("path", "BPCellsMaskSeed", function(object) {
    c(path(object@matrix), path(object@mask))
})

#' @export
#' @rdname showtree
methods::setMethod("seed", "BPCellsMaskSeed", function(x) {
    list(matrix = seed(x@matrix), mask = seed(x@mask))
})

#' @export
#' @rdname showtree
methods::setMethod("nseed", "BPCellsMaskSeed", function(x) {
    sum(nseed(x@matrix), nseed(x@mask))
})

###########################################################
#' @importFrom DelayedArray path
#' @export
#' @rdname showtree
methods::setMethod("path", "BPCellsMultiplySeed", function(object) {
    c(path(object@left), path(object@right))
})

#' @export
#' @rdname showtree
methods::setMethod("seed", "BPCellsMultiplySeed", function(x) {
    list(left = seed(x@left), right = seed(x@right))
})

#' @export
#' @rdname showtree
methods::setMethod("nseed", "BPCellsMultiplySeed", function(x) {
    sum(nseed(x@left), nseed(x@right))
})

# Five DelayedUnaryOp-like seeds ------------------------------------
###########################################################
#' @importFrom DelayedArray path
#' @export
#' @rdname showtree
methods::setMethod("path", "BPCellsUnaryOpsSeed", function(object) {
    path(object@matrix)
})

#' @export
#' @rdname showtree
methods::setMethod("seed", "BPCellsUnaryOpsSeed", function(x) {
    seed(x@matrix)
})

#' @export
#' @rdname showtree
methods::setMethod("nseed", "BPCellsUnaryOpsSeed", function(x) {
    nseed(x@matrix)
})

# Three basic seeds ------------------------------------------
###########################################################
#' @importFrom DelayedArray path
#' @export
#' @rdname showtree
methods::setMethod("path", "BPCellsMemSeed", function(object) {
    character()
})
#' @export
#' @rdname showtree
methods::setMethod("seed", "BPCellsMemSeed", function(x) x)

#' @export
#' @rdname showtree
methods::setMethod("nseed", "BPCellsMemSeed", function(x) 1L)

###########################################################
#' @importFrom DelayedArray path
#' @export
#' @rdname showtree
methods::setMethod("path", "BPCellsdgCMatrixSeed", function(object) {
    character()
})

#' @export
#' @rdname showtree
methods::setMethod("seed", "BPCellsdgCMatrixSeed", function(x) x)

#' @export
#' @rdname showtree
methods::setMethod("nseed", "BPCellsdgCMatrixSeed", function(x) 1L)

###########################################################
#' @importFrom DelayedArray path
#' @export
#' @rdname showtree
methods::setMethod("path", "BPCellsDirSeed", function(object) object@dir)

#' @export
#' @rdname showtree
methods::setMethod("seed", "BPCellsDirSeed", function(x) x)

#' @export
#' @rdname showtree
methods::setMethod("nseed", "BPCellsDirSeed", function(x) 1L)
