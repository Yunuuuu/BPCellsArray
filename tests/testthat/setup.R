# Creating temporary directory
tmpdir <- testthat::test_path("_TEMP")
if (!dir.exists(tmpdir)) dir.create(tmpdir)
tmpdir <- normalizePath(tmpdir, mustWork = TRUE)

test_methods <- function(obj, ...) {
    with_seedform("BPCells", methods_test(obj = obj, ...))
    with_seedform("DelayedArray", methods_test(obj = obj, ...))
}

methods_test <- function(
    obj, ..., mode = NULL, mat = NULL,
    name, skip_multiplication = FALSE) {
    mode <- mode %||% storage_mode(obj)
    mat <- mat %||% as.matrix(obj)
    mat <- convert_mode(mat, mode)

    ########################################################
    cli::cli_inform("{.field convert_mode} for seed {name} works as expected")
    testthat::test_that(
        sprintf("`convert_mode()` for seed %s works as expected", name),
        {
            obj <- BPCellsMatrix(obj)
            # float mode
            float_obj <- convert_mode(obj, "float")
            testthat::expect_s4_class(float_obj, "BPCellsMatrix")
            testthat::expect_identical(storage_mode(float_obj), "float")
            float_mat <- convert_mode(mat, "float")
            testthat::expect_equal(as.matrix(float_obj), float_mat)
            # double mode
            double_obj <- convert_mode(obj, "double")
            testthat::expect_s4_class(double_obj, "BPCellsMatrix")
            testthat::expect_identical(storage_mode(double_obj), "double")
            double_mat <- convert_mode(mat, "double")
            testthat::expect_equal(as.matrix(double_obj), double_mat)
            # integer mode
            integer_obj <- convert_mode(obj, "uint32_t")
            testthat::expect_s4_class(integer_obj, "BPCellsMatrix")
            testthat::expect_identical(storage_mode(integer_obj), "uint32_t")
            integer_mat <- convert_mode(mat, "uint32_t")
            testthat::expect_equal(as.matrix(integer_obj), integer_mat)
        }
    )

    ########################################################
    cli::cli_inform("{.field t} for seed {name} works as expected")
    testthat::test_that(
        sprintf("`t()` for seed %s works as expected", name),
        {
            obj1 <- BPCellsMatrix(obj)
            testthat::expect_identical(storage_axis(obj1), "col")
            obj2 <- t(obj1)
            testthat::expect_s4_class(obj2, "BPCellsMatrix")
            testthat::expect_identical(storage_axis(obj2), "row")
            testthat::expect_equal(as.matrix(obj2), t(mat))
        }
    )

    cli::cli_inform("{.field transpose_axis} for seed {name} works as expected")
    testthat::test_that(
        sprintf("`transpose_axis()` for seed %s works as expected", name),
        {
            obj <- BPCellsMatrix(obj)
            testthat::expect_identical(storage_axis(obj), "col")

            # for untransposed matrix
            obj1 <- transpose_axis(obj)
            testthat::expect_s4_class(obj1, "BPCellsMatrix")
            testthat::expect_identical(storage_axis(obj1), "row")
            testthat::expect_identical(as.matrix(obj1), as.matrix(obj))
            testthat::expect_equal(as.matrix(obj1), mat)
            # for transposed matrix
            obj <- t(obj)
            obj2 <- transpose_axis(obj)
            testthat::expect_s4_class(obj2, "BPCellsMatrix")
            testthat::expect_identical(storage_axis(obj2), "col")
            testthat::expect_identical(as.matrix(obj2), as.matrix(obj))
            testthat::expect_equal(as.matrix(obj2), t(mat))
        }
    )
    ########################################################
    cli::cli_inform("{.field subset} seed {name} works as expected")
    testthat::test_that(
        sprintf("`[` of `i, missing` for %s works as expected", name),
        {
            array <- BPCellsMatrix(obj)
            slice <- array[1:10, ]
            testthat::expect_s4_class(slice, "BPCellsMatrix")
            testthat::expect_identical(rownames(slice), rownames(array)[1:10])
            testthat::expect_identical(colnames(slice), colnames(array))
            testthat::expect_equal(as.matrix(slice), mat[1:10, ])
        }
    )
    testthat::test_that(
        sprintf("`[` of `missing,j` for seed %s works as expected", name),
        {
            array <- BPCellsMatrix(obj)
            slice <- array[, 1:10]
            testthat::expect_s4_class(slice, "BPCellsMatrix")
            testthat::expect_identical(colnames(slice), colnames(array)[1:10])
            testthat::expect_identical(rownames(slice), rownames(array))
            testthat::expect_equal(as.matrix(slice), mat[, 1:10])
        }
    )
    testthat::test_that(
        sprintf("`[` of `i,j` for seed %s works as expected", name),
        {
            array <- BPCellsMatrix(obj)
            slice <- array[1:10, 1:10]
            testthat::expect_s4_class(slice, "BPCellsMatrix")
            testthat::expect_identical(rownames(slice), rownames(array)[1:10])
            testthat::expect_identical(colnames(slice), colnames(array)[1:10])
            testthat::expect_equal(as.matrix(slice), mat[1:10, 1:10])
        }
    )

    #################################################################
    values <- matrix(sample(mat, length(mat)), nrow = nrow(mat))
    if (any(as.matrix(obj) > .Machine$integer.max)) {
        modes <- "double"
    } else {
        modes <- c("uint32_t", "double")
    }
    for (mm in modes) {
        cli::cli_inform("{.field [<-} for {mm} seed {name} works as expected")
        testthat::test_that(
            sprintf(
                "`[<-()` of `i,missing` for %s seed %s works as expected",
                mm, name
            ),
            {
                array <- BPCellsMatrix(obj)
                array <- convert_mode(array, mm)
                testthat::expect_identical(storage_mode(array), mm)
                suppressWarnings(array[1:10, ] <- values[1:10, ])
                testthat::expect_s4_class(array, "BPCellsMatrix")
                testthat::expect_identical(storage_mode(array), mm)
                mat[1:10, ] <- values[1:10, ]
                mat <- convert_mode(mat, mm)
                testthat::expect_equal(as.matrix(array), mat)
            }
        )
        testthat::test_that(
            sprintf(
                "`[<-()` of `missing,j` for %s %s works as expected",
                mm, name
            ),
            {
                array <- BPCellsMatrix(obj)
                array <- convert_mode(array, mm)
                testthat::expect_identical(storage_mode(array), mm)
                suppressWarnings(array[, 1:10] <- values[, 1:10])
                testthat::expect_s4_class(array, "BPCellsMatrix")
                testthat::expect_identical(storage_mode(array), mm)
                mat[, 1:10] <- values[, 1:10]
                mat <- convert_mode(mat, mm)
                testthat::expect_equal(as.matrix(array), mat)
            }
        )
        testthat::test_that(
            sprintf(
                "`[<-()` of `i,j` for %s %s works as expected",
                mm, name
            ),
            {
                array <- BPCellsMatrix(obj)
                array <- convert_mode(array, mm)
                testthat::expect_identical(storage_mode(array), mm)
                suppressWarnings(array[1:10, 1:5] <- values[1:10, 1:5])
                testthat::expect_s4_class(array, "BPCellsMatrix")
                testthat::expect_identical(storage_mode(array), mm)
                mat[1:10, 1:5] <- values[1:10, 1:5]
                mat <- convert_mode(mat, mm)
                testthat::expect_equal(as.matrix(array), mat)
            }
        )
    }

    cli::cli_inform("{.field dimnames<-} for seed {name} works as expected")
    testthat::test_that(
        sprintf("`dimnames<-()` for seed %s works as expected", name),
        {
            obj <- BPCellsArray(obj)
            testthat::expect_s4_class(obj, "BPCellsMatrix")
            nms <- list(
                paste0("G", seq_len(nrow(obj))),
                paste0("C", seq_len(ncol(obj)))
            )
            dimnames(obj) <- nms
            testthat::expect_identical(dimnames(obj), nms)
            testthat::expect_s4_class(obj, "BPCellsMatrix")
        }
    )

    cli::cli_inform("{.field rownames<-} for seed {name} works as expected")
    testthat::test_that(
        sprintf("`rownames<-()` for seed %s works as expected", name),
        {
            obj <- BPCellsMatrix(obj)
            testthat::expect_s4_class(obj, "BPCellsMatrix")
            testthat::expect_error(rownames(obj) <- list())
            testthat::expect_error(rownames(obj) <- 1:10)
            rownames(obj) <- paste0("G", seq_len(nrow(obj)))
            testthat::expect_s4_class(obj, "BPCellsMatrix")
            testthat::expect_identical(
                rownames(obj),
                paste0("G", seq_len(nrow(obj)))
            )
            rownames(obj) <- NULL
            testthat::expect_s4_class(obj, "BPCellsMatrix")
            testthat::expect_null(rownames(obj))
        }
    )

    cli::cli_inform("{.field colnames<-} for seed {name} works as expected")
    testthat::test_that(
        sprintf("`colnames<-()` for seed %s works as expected", name),
        {
            obj <- BPCellsMatrix(obj)
            testthat::expect_s4_class(obj, "BPCellsMatrix")
            testthat::expect_error(colnames(obj) <- list())
            testthat::expect_error(colnames(obj) <- 1:10)
            colnames(obj) <- paste0("G", seq_len(ncol(obj)))
            testthat::expect_s4_class(obj, "BPCellsMatrix")
            testthat::expect_identical(
                colnames(obj),
                paste0("G", seq_len(ncol(obj)))
            )
            colnames(obj) <- NULL
            testthat::expect_s4_class(obj, "BPCellsMatrix")
            testthat::expect_null(colnames(obj))
        }
    )

    cli::cli_inform("{.field rank_transform} for seed {name} works as expected")
    testthat::test_that(
        sprintf("`rank_transform` for seed %s works as expected", name),
        {
            obj <- BPCellsArray(obj)
            testthat::expect_equal(
                as.matrix(suppressWarnings(rowRanks(obj, offset = FALSE))),
                rowRanks(mat, ties.method = "average", useNames = TRUE)
            )
            testthat::expect_equal(
                as.matrix(suppressWarnings(colRanks(obj, offset = FALSE))),
                colRanks(mat,
                    ties.method = "average",
                    useNames = TRUE, preserveShape = TRUE
                )
            )
        }
    )

    cli::cli_inform("{.field %*%} for seed {name} works as expected")
    testthat::test_that(
        sprintf("`%%*%%` for seed %s works as expected", name),
        {
            testthat::skip_if(
                skip_multiplication,
                "Skipping for TransformedMatrix"
            )
            obj <- BPCellsArray(obj)
            testthat::expect_s4_class(obj, "BPCellsMatrix")
            testthat::expect_warning(temp <- obj %*% t(obj))
            testthat::expect_equal(as.matrix(temp), mat %*% t(mat))
            testthat::expect_s4_class(temp, "BPCellsMatrix")
            testthat::expect_true(is.matrix(obj %*% as.matrix(t(obj))))
            testthat::expect_true(is.matrix(obj %*% seq_len(ncol(obj))))
            testthat::expect_true(is.matrix(seq_len(nrow(obj)) %*% obj))
        }
    )

    cli::cli_inform("{.field rbind} for seed {name} works as expected")
    testthat::test_that(
        sprintf("`rbind()` for seed %s works as expected", name),
        {
            obj <- BPCellsArray(obj)
            testthat::expect_s4_class(obj, "BPCellsMatrix")
            testthat::expect_s4_class(rbind2(obj, obj), "BPCellsMatrix")
            testthat::expect_s4_class(rbind(obj, obj), "BPCellsMatrix")
            testthat::expect_s4_class(arbind(obj, obj), "BPCellsMatrix")
            testthat::expect_s4_class(
                bindROWS(obj, list(obj)),
                "BPCellsMatrix"
            )
            testthat::expect_equal(as.matrix(rbind2(obj, obj)), rbind(mat, mat))
            testthat::expect_equal(as.matrix(rbind(obj, obj)), rbind(mat, mat))
            testthat::expect_equal(as.matrix(arbind(obj, obj)), rbind(mat, mat))
            testthat::expect_equal(
                as.matrix(bindROWS(obj, list(obj))),
                rbind(mat, mat)
            )
        }
    )

    cli::cli_inform("{.field cbind} for seed {name} works as expected")
    testthat::test_that(
        sprintf("`cbind()` for seed %s works as expected", name),
        {
            obj <- BPCellsArray(obj)
            testthat::expect_s4_class(obj, "BPCellsMatrix")
            testthat::expect_s4_class(cbind2(obj, obj), "BPCellsMatrix")
            testthat::expect_equal(as.matrix(cbind2(obj, obj)), cbind(mat, mat))
            testthat::expect_s4_class(cbind(obj, obj), "BPCellsMatrix")
            testthat::expect_equal(as.matrix(cbind(obj, obj)), cbind(mat, mat))
            testthat::expect_s4_class(acbind(obj, obj), "BPCellsMatrix")
            testthat::expect_equal(as.matrix(acbind(obj, obj)), cbind(mat, mat))
            testthat::expect_s4_class(
                bindCOLS(obj, list(obj)),
                "BPCellsMatrix"
            )
            testthat::expect_equal(
                as.matrix(bindCOLS(obj, list(obj))),
                cbind(mat, mat)
            )
        }
    )

    cli::cli_inform("{.field +} for seed {name} works as expected")
    testthat::test_that(
        sprintf("`+` for seed %s works as expected", name),
        {
            obj <- BPCellsArray(obj)
            testthat::expect_s4_class(obj, "BPCellsMatrix")
            testthat::expect_s4_class(obj + 1, "BPCellsMatrix")
            testthat::expect_equal(as.matrix(obj + 1), mat + 1)
            testthat::expect_s4_class(obj + 1 + 10, "BPCellsMatrix")
            testthat::expect_equal(as.matrix(obj + 1 + 10), mat + 1 + 10)
            testthat::expect_s4_class(
                obj + seq_len(nrow(obj)),
                "BPCellsMatrix"
            )
            testthat::expect_equal(
                as.matrix(obj + seq_len(nrow(obj))),
                mat + seq_len(nrow(obj))
            )
            testthat::expect_s4_class(
                t(obj) + seq_len(ncol(obj)),
                "BPCellsMatrix"
            )
            testthat::expect_equal(
                as.matrix(t(obj) + seq_len(ncol(obj))),
                t(mat) + seq_len(ncol(obj))
            )
            testthat::expect_s4_class(1 + obj, "BPCellsMatrix")
            testthat::expect_s4_class(
                seq_len(nrow(obj)) + obj,
                "BPCellsMatrix"
            )
            testthat::expect_equal(
                as.matrix(seq_len(nrow(obj)) + obj),
                seq_len(nrow(obj)) + mat
            )
            testthat::expect_s4_class(
                seq_len(ncol(obj)) + t(obj),
                "BPCellsMatrix"
            )
            testthat::expect_equal(
                as.matrix(seq_len(ncol(obj)) + t(obj)),
                seq_len(ncol(obj)) + t(mat)
            )
        }
    )

    cli::cli_inform("{.field -} for seed {name} works as expected")
    testthat::test_that(
        sprintf("`-` for seed %s works as expected", name),
        {
            obj <- BPCellsArray(obj)
            testthat::expect_s4_class(obj, "BPCellsMatrix")
            testthat::expect_s4_class(obj - 1, "BPCellsMatrix")
            testthat::expect_equal(as.matrix(obj - 1), mat - 1)
            testthat::expect_s4_class(obj - 1 - 10, "BPCellsMatrix")
            testthat::expect_equal(as.matrix(obj - 1 - 10), mat - 1 - 10)
            testthat::expect_s4_class(
                obj - seq_len(nrow(obj)),
                "BPCellsMatrix"
            )
            testthat::expect_equal(
                as.matrix(obj - seq_len(nrow(obj))),
                mat - seq_len(nrow(obj))
            )
            testthat::expect_s4_class(
                t(obj) - seq_len(ncol(obj)),
                "BPCellsMatrix"
            )
            testthat::expect_equal(
                as.matrix(t(obj) - seq_len(ncol(obj))),
                t(mat) - seq_len(ncol(obj))
            )
            testthat::expect_s4_class(1 - obj, "BPCellsMatrix")
            testthat::expect_s4_class(
                seq_len(nrow(obj)) - obj,
                "BPCellsMatrix"
            )
            testthat::expect_equal(
                as.matrix(seq_len(nrow(obj)) - obj),
                seq_len(nrow(obj)) - mat
            )
            testthat::expect_s4_class(
                seq_len(ncol(obj)) - t(obj),
                "BPCellsMatrix"
            )
            testthat::expect_equal(
                as.matrix(seq_len(ncol(obj)) - t(obj)),
                seq_len(ncol(obj)) - t(mat)
            )
        }
    )

    cli::cli_inform("{.field *} for seed {name} works as expected")
    testthat::test_that(
        sprintf("`*` for seed %s works as expected", name),
        {
            obj <- BPCellsArray(obj)
            testthat::expect_s4_class(obj, "BPCellsMatrix")
            testthat::expect_s4_class(obj * 1, "BPCellsMatrix")
            testthat::expect_equal(as.matrix(obj * 1), mat * 1)
            testthat::expect_s4_class(obj * 1 * 10, "BPCellsMatrix")
            testthat::expect_equal(as.matrix(obj * 1 * 10), mat * 1 * 10)
            testthat::expect_s4_class(
                obj * seq_len(nrow(obj)),
                "BPCellsMatrix"
            )
            testthat::expect_equal(
                as.matrix(obj * seq_len(nrow(obj))),
                mat * seq_len(nrow(obj))
            )
            testthat::expect_s4_class(
                t(obj) * seq_len(ncol(obj)),
                "BPCellsMatrix"
            )
            testthat::expect_equal(
                as.matrix(t(obj) * seq_len(ncol(obj))),
                t(mat) * seq_len(ncol(obj))
            )
            testthat::expect_s4_class(1 * obj, "BPCellsMatrix")
            testthat::expect_s4_class(
                seq_len(nrow(obj)) * obj,
                "BPCellsMatrix"
            )
            testthat::expect_equal(
                as.matrix(seq_len(nrow(obj)) * obj),
                seq_len(nrow(obj)) * mat
            )
            testthat::expect_s4_class(
                seq_len(ncol(obj)) * t(obj),
                "BPCellsMatrix"
            )
            testthat::expect_equal(
                as.matrix(seq_len(ncol(obj)) * t(obj)),
                seq_len(ncol(obj)) * t(mat)
            )
        }
    )

    cli::cli_inform("{.field /} for seed {name} works as expected")
    testthat::test_that(
        sprintf("`/` for seed %s works as expected", name),
        {
            obj <- BPCellsArray(obj)
            testthat::expect_s4_class(obj, "BPCellsMatrix")
            testthat::expect_s4_class(obj / 1, "BPCellsMatrix")
            testthat::expect_equal(as.matrix(obj / 1), mat / 1)
            testthat::expect_s4_class(obj / 1 / 10, "BPCellsMatrix")
            testthat::expect_equal(as.matrix(obj / 1 / 10), mat / 1 / 10)
            testthat::expect_s4_class(
                obj / seq_len(nrow(obj)),
                "BPCellsMatrix"
            )
            testthat::expect_equal(
                as.matrix(obj / seq_len(nrow(obj))),
                mat / seq_len(nrow(obj))
            )
            testthat::expect_s4_class(
                t(obj) / seq_len(ncol(obj)),
                "BPCellsMatrix"
            )
            testthat::expect_equal(
                as.matrix(t(obj) / seq_len(ncol(obj))),
                t(mat) / seq_len(ncol(obj))
            )
            testthat::expect_error(1 / obj)
        }
    )
}
