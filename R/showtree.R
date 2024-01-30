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
#'  - `path`: An atomic characters or a list of characters.
#'  - `nseed`: An integer number.
#'  - `seed`: A list of [BPCellsSeed][BPCellsSeed-class] objects or a single
#'    [BPCellsSeed][BPCellsSeed-class] object.
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
        x_as1line <- summary(x)
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
        Recall(x = entity(x), indent = indent, last.child = TRUE)
    } else if (methods::is(x, "BPCellsNaryOpsSeed")) {
        x <- entity(x)
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
    x <- x@seed
    methods::callGeneric()
})

#' @importFrom DelayedArray seed
#' @export
#' @rdname showtree
methods::setMethod("seed", "BPCellsMatrix", function(x) {
    x <- x@seed
    methods::callGeneric()
})

#' @importFrom DelayedArray path
#' @export
#' @rdname showtree
methods::setMethod("path", "BPCellsMatrix", function(object, ...) {
    object <- object@seed
    methods::callGeneric()
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

# here: we allow `seed` function to return a list of seeds object, which is
# different with what DelayedArray dose, this is because that seedApply()
# function is not a generic function, we cannot define methods for it.

# Four basic seeds ------------------------------------------
###########################################################
#' @importFrom DelayedArray path
#' @export
#' @rdname showtree
methods::setMethod("path", "BPCellsMemSeed", function(object, ...) {
    character()
})

#' @importFrom DelayedArray path
#' @export
#' @rdname showtree
methods::setMethod("path", "BPCellsdgCMatrixSeed", function(object, ...) {
    character()
})

#' @importFrom DelayedArray path
#' @export
#' @rdname showtree
methods::setMethod("path", "BPCellsDirSeed", function(object, ...) object@dir)

#' @export
#' @rdname showtree
methods::setMethod("path", "BPCellsHDF5Seed", function(object, ...) object@path)

#' @export
#' @rdname showtree
methods::setMethod("seed", "BPCellsBasicSeed", function(x) x)

#' @export
#' @rdname showtree
methods::setMethod("nseed", "BPCellsBasicSeed", function(x) 1L)

# Five DelayedUnaryOp-like seeds ------------------------------------
###########################################################
#' @importFrom DelayedArray path
#' @export
#' @rdname showtree
methods::setMethod("path", "BPCellsUnaryOpsSeed", function(object, ...) {
    path(entity(object), ...)
})

#' @export
#' @rdname showtree
methods::setMethod("seed", "BPCellsUnaryOpsSeed", function(x) {
    seed(entity(x))
})

#' @export
#' @rdname showtree
methods::setMethod("nseed", "BPCellsUnaryOpsSeed", function(x) {
    nseed(entity(x))
})

# Three DelayedNaryOp-like seeds ----------------------------------------
###########################################################
#' @param ... Additional arguments passed to other methods.
#' @param unlist A bool, if `TRUE`, will unlist the output, used to return a
#' character vector for `path` function, otherwise, `path` function will return
#' a list of path.
#' @importFrom DelayedArray path
#' @export
#' @rdname showtree
methods::setMethod("path", "BPCellsNaryOpsSeed", function(object, unlist = TRUE) {
    out <- lapply(entity(object), path)
    if (isTRUE(unlist)) out <- unlist(out, recursive = FALSE, use.names = FALSE)
    out
})

#' @importFrom DelayedArray seed
#' @export
#' @rdname showtree
methods::setMethod("seed", "BPCellsNaryOpsSeed", function(x) {
    unlist(lapply(entity(x), seed), recursive = FALSE, use.names = FALSE)
})

#' @export
#' @rdname showtree
methods::setMethod("nseed", "BPCellsNaryOpsSeed", function(x) {
    sum(vapply(entity(x), nseed, integer(1L), USE.NAMES = FALSE))
})

# Don't use this, we directly use `seed` function to return all seeds
# this is different with what the DelayedArray does.
methods::setGeneric("new_seedApply",
    signature = "x", function(x, .fn, ...) {
        standardGeneric("new_seedApply")
    }
)

methods::setMethod("new_seedApply", "BPCellsMatrix", function(x, .fn, ...) {
    x <- x@seed
    methods::callGeneric()
})

methods::setMethod(
    "new_seedApply", "BPCellsUnaryOpsSeed",
    function(x, .fn, ...) {
        x <- entity(x)
        methods::callGeneric()
    }
)

methods::setMethod(
    "new_seedApply", "BPCellsNaryOpsSeed",
    function(x, .fn, ...) {
        x <- entity(x)
        ans <- lapply(x, FUN = new_seedApply, .fn = .fn, ...)
        unlist(ans, recursive = FALSE, use.names = FALSE)
    }
)

methods::setMethod("new_seedApply", "BPCellsBasicSeed", function(x, .fn, ...) {
    list(.fn(x, ...))
})
