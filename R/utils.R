`%||%` <- function(x, y) if (is.null(x)) y else x

pkg_nm <- function() {
    utils::packageName(topenv(environment()))
}

recode <- function(x, replace) {
    old_values <- names(replace)
    missing_values <- setdiff(old_values, x)
    if (length(missing_values) > 0) {
        keep <- !old_values %in% missing_values
        replace <- replace[keep]
        old_values <- old_values[keep]
    }
    x[match(old_values, x)] <- as.vector(replace)
    x
}

rename <- function(x, replace) {
    names(x) <- recode(names(x), replace)
    x
}

c_msg <- function(..., sep = " ") {
    paste(..., sep = sep)
}

fclass <- function(object) class(object)[1L]

list_methods <- function(class, where = asNamespace("DelayedArray"), ...) {
    fns <- methods::showMethods(...,
        classes = class, where = where, printTo = FALSE
    )
    gsub(
        "^Function(\\:\\s|\\s\\\")([^\\s]+)(\\s\\(|\\\")(.+$)",
        "\\2", grep("^Function", fns, value = TRUE, perl = TRUE),
        perl = TRUE
    )
}

imap <- function(.x, .f, ...) {
    .mapply(.f, list(.x, names(.x) %||% seq_along(.x)), list(...))
}

matrix_to_integer <- function(matrix) { # a numeric matrix
    if (is.integer(matrix)) return(matrix) # styler: off
    if (all(matrix <= .Machine$integer.max)) {
        storage.mode(matrix) <- "integer"
    } else {
        cli::cli_warn(
            "Using `double` mode since some values exceed {.code .Machine$integer.max}"
        )
    }
    matrix
}

matrix_to_double <- function(matrix) { # a numeric matrix
    if (is.double(matrix)) return(matrix) # styler: off
    storage.mode(matrix) <- "double"
    matrix
}

########################################################
# running order: before + method + after
new_method <- function(args, method, before = NULL, after = NULL) {
    if (!is.null(after)) {
        method <- substitute(object <- method, list(method = method)) # nolint
    }
    body <- list(method)
    if (!is.null(before)) body <- c(before, body)
    if (!is.null(after)) body <- c(body, after)
    body <- as.call(c(as.name("{"), body))
    rlang::new_function(args, body = body)
}

# Use chartr() for safety since toupper() fails to convert i to I in Turkish
# locale
lower_ascii <- "abcdefghijklmnopqrstuvwxyz"
upper_ascii <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

snake_class <- function(x) {
    snakeize(class(x)[1])
}

snakeize <- function(x) {
    x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
    x <- gsub(".", "_", x, fixed = TRUE)
    x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
    to_lower_ascii(x)
}
to_lower_ascii <- function(x) chartr(upper_ascii, lower_ascii, x)
to_upper_ascii <- function(x) chartr(lower_ascii, upper_ascii, x)
