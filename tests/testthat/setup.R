# Creating temporary directory
tmpdir <- testthat::test_path("_TEMP")
if (!dir.exists(tmpdir)) dir.create(tmpdir)
tmpdir <- normalizePath(tmpdir, mustWork = TRUE)

common_test <- function(mat, obj, actual_path, seed_fn, name) {
    seed_class <- sprintf("BPCells%sSeed", name)
    seed_name <- sprintf("BPCellsSeed class of `%s`", name)
    matrix_name <- sprintf("BPCellsMatrix class of `%s`", name)
    testthat::test_that(
        sprintf("`BPCells%sSeed()` works as expected", name),
        {
            seed <- seed_fn(obj)
            testthat::expect_s4_class(seed, seed_class)
            testthat::expect_equal(as.matrix(seed), mat)
            obj <- DelayedArray(seed)
            testthat::expect_s4_class(obj, "BPCellsMatrix")
            testthat::expect_equal(as.matrix(obj), mat)
            testthat::expect_identical(path(seed), actual_path)
            testthat::expect_identical(path(obj), actual_path)
        }
    )

    testthat::test_that(
        sprintf("`subset()` %s works as expected", seed_name),
        {
            seed <- seed_fn(obj)
            testthat::expect_s4_class(seed[1:10, ], "BPCellsSeed")
            testthat::expect_equal(as.matrix(seed[1:10, ]), mat[1:10, ])
            testthat::expect_s4_class(seed[, 1:10], "BPCellsSeed")
            testthat::expect_equal(as.matrix(seed[, 1:10]), mat[, 1:10])
            testthat::expect_s4_class(seed[1:10, 1:10], "BPCellsSeed")
            testthat::expect_equal(as.matrix(seed[1:10, 1:10]), mat[1:10, 1:10])
        }
    )

    testthat::test_that(
        sprintf("`subset()` %s works as expected", matrix_name),
        {
            obj <- BPCellsArray(obj)
            testthat::expect_s4_class(obj[1:10, ], "BPCellsMatrix")
            testthat::expect_equal(as.matrix(obj[1:10, ]), mat[1:10, ])
            testthat::expect_s4_class(obj[, 1:10], "BPCellsMatrix")
            testthat::expect_equal(as.matrix(obj[, 1:10]), mat[, 1:10])
            testthat::expect_s4_class(obj[1:10, 1:10], "BPCellsMatrix")
            testthat::expect_equal(as.matrix(obj[1:10, 1:10]), mat[1:10, 1:10])
        }
    )

    testthat::test_that(
        sprintf("`convert_type()` %s works as expected", seed_name),
        {
            seed <- seed_fn(obj)
            float_seed <- convert_type(seed, "numeric")
            testthat::expect_s4_class(float_seed, "BPCellsConvertSeed")
            testthat::expect_identical(type(float_seed), "double")
            integer_seed <- convert_type(float_seed, "integer")
            testthat::expect_s4_class(integer_seed, "BPCellsConvertSeed")
            testthat::expect_identical(type(integer_seed), "integer")
        }
    )

    testthat::test_that(
        sprintf("`convert_type()` %s works as expected", matrix_name),
        {
            obj <- BPCellsArray(obj)
            float_obj <- convert_type(obj, "numeric")
            testthat::expect_s4_class(float_obj, "BPCellsMatrix")
            testthat::expect_identical(type(float_obj), "double")
            integer_obj <- convert_type(float_obj, "integer")
            testthat::expect_s4_class(integer_obj, "BPCellsMatrix")
            testthat::expect_identical(type(integer_obj), "integer")
        }
    )

    testthat::test_that(
        sprintf("`t()` BPCells%s works as expected", name),
        {
            seed <- seed_fn(obj)
            testthat::expect_s4_class(seed, seed_class)
            testthat::expect_s4_class(t(seed), seed_class)
            testthat::expect_equal(as.matrix(t(seed)), t(mat))
            obj <- BPCellsArray(obj)
            testthat::expect_s4_class(t(obj), "BPCellsMatrix")
            testthat::expect_equal(as.matrix(t(obj)), t(mat))
        }
    )

    testthat::test_that(
        sprintf("`dimnames<-()` BPCells%s works as expected", name),
        {
            seed <- seed_fn(obj)
            testthat::expect_s4_class(seed, seed_class)
            dimnames(seed) <- list(
                paste0("G", seq_len(nrow(seed))),
                paste0("C", seq_len(ncol(seed)))
            )
            testthat::expect_s4_class(seed, "BPCellsSeed")
            obj <- BPCellsArray(obj)
            testthat::expect_s4_class(obj, "BPCellsMatrix")
            dimnames(obj) <- list(
                paste0("G", seq_len(nrow(obj))),
                paste0("C", seq_len(ncol(obj)))
            )
            testthat::expect_s4_class(obj, "BPCellsMatrix")
        }
    )

    testthat::test_that(
        sprintf("`rownames<-()` BPCells%s works as expected", name),
        {
            seed <- seed_fn(obj)
            testthat::expect_s4_class(seed, seed_class)
            testthat::expect_error(rownames(seed) <- list())
            testthat::expect_error(rownames(seed) <- 1:10)
            rownames(seed) <- paste0("G", seq_len(nrow(seed)))
            testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
            testthat::expect_identical(rownames(seed), paste0("G", seq_len(nrow(seed))))
            obj <- BPCellsMatrix(obj)
            testthat::expect_s4_class(obj, "BPCellsMatrix")
            testthat::expect_error(rownames(obj) <- list())
            testthat::expect_error(rownames(obj) <- 1:10)
            rownames(obj) <- paste0("G", seq_len(nrow(obj)))
            testthat::expect_s4_class(obj, "BPCellsMatrix")
            testthat::expect_identical(rownames(obj), paste0("G", seq_len(nrow(obj))))
            rownames(seed) <- NULL
            testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
            testthat::expect_null(rownames(seed))
            rownames(obj) <- NULL
            testthat::expect_s4_class(obj, "BPCellsMatrix")
            testthat::expect_null(rownames(obj))
        }
    )

    testthat::test_that(
        sprintf("`colnames<-()` BPCells%s works as expected", name),
        {
            seed <- seed_fn(obj)
            testthat::expect_s4_class(seed, seed_class)
            testthat::expect_error(colnames(seed) <- list())
            testthat::expect_error(colnames(seed) <- 1:10)
            colnames(seed) <- paste0("G", seq_len(ncol(seed)))
            testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
            testthat::expect_identical(
                colnames(seed),
                paste0("G", seq_len(ncol(seed)))
            )
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
            colnames(seed) <- NULL
            testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
            testthat::expect_null(colnames(seed))
            colnames(obj) <- NULL
            testthat::expect_s4_class(obj, "BPCellsMatrix")
            testthat::expect_null(colnames(obj))
        }
    )

    testthat::test_that(
        sprintf("`%%*%%` BPCells%s works as expected", name),
        {
            seed <- seed_fn(obj)
            testthat::expect_s4_class(seed, seed_class)
            testthat::expect_warning(temp <- seed %*% t(seed))
            testthat::expect_equal(as.matrix(temp), mat %*% t(mat))
            testthat::expect_s4_class(temp, "BPCellsMultiplySeed")
            testthat::expect_true(is.matrix(seed %*% as.matrix(t(seed))))
            testthat::expect_true(is.matrix(seed %*% seq_len(ncol(seed))))
            testthat::expect_true(is.matrix(seq_len(nrow(seed)) %*% seed))
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

    testthat::test_that(
        sprintf("`rbind()` %s works as expected", seed_name),
        {
            seed <- seed_fn(obj)
            testthat::expect_s4_class(seed, seed_class)
            testthat::expect_s4_class(
                rbind2(seed, seed),
                "BPCellsRowBindMatrixSeed"
            )
            testthat::expect_equal(
                as.matrix(rbind2(seed, seed)),
                rbind(mat, mat)
            )
            testthat::expect_s4_class(
                rbind(seed, seed),
                "BPCellsRowBindMatrixSeed"
            )
            testthat::expect_equal(
                as.matrix(rbind(seed, seed)),
                rbind(mat, mat)
            )
            testthat::expect_s4_class(
                arbind(seed, seed),
                "BPCellsRowBindMatrixSeed"
            )
            testthat::expect_equal(
                as.matrix(arbind(seed, seed)),
                rbind(mat, mat)
            )
            testthat::expect_s4_class(
                bindROWS(seed, list(seed)),
                "BPCellsRowBindMatrixSeed"
            )
            testthat::expect_equal(
                as.matrix(bindROWS(seed, list(seed))),
                rbind(mat, mat)
            )
        }
    )

    testthat::test_that(
        sprintf("`rbind()` %s works as expected", matrix_name),
        {
            obj <- BPCellsArray(obj)
            testthat::expect_s4_class(obj, "BPCellsMatrix")
            testthat::expect_s4_class(rbind2(obj, obj), "BPCellsMatrix")
            testthat::expect_equal(as.matrix(rbind2(obj, obj)), rbind(mat, mat))
            testthat::expect_s4_class(rbind(obj, obj), "BPCellsMatrix")
            testthat::expect_equal(as.matrix(rbind(obj, obj)), rbind(mat, mat))
            testthat::expect_s4_class(arbind(obj, obj), "BPCellsMatrix")
            testthat::expect_equal(as.matrix(arbind(obj, obj)), rbind(mat, mat))
            testthat::expect_s4_class(
                bindROWS(obj, list(obj)),
                "BPCellsMatrix"
            )
            testthat::expect_equal(
                as.matrix(bindROWS(obj, list(obj))),
                rbind(mat, mat)
            )
        }
    )

    testthat::test_that(
        sprintf("`cbind()` %s works as expected", seed_name),
        {
            seed <- seed_fn(obj)
            testthat::expect_s4_class(seed, seed_class)
            testthat::expect_s4_class(
                cbind2(seed, seed),
                "BPCellsColBindMatrixSeed"
            )
            testthat::expect_equal(
                as.matrix(cbind2(seed, seed)),
                cbind(mat, mat)
            )
            testthat::expect_s4_class(
                cbind(seed, seed),
                "BPCellsColBindMatrixSeed"
            )
            testthat::expect_equal(
                as.matrix(cbind(seed, seed)),
                cbind(mat, mat)
            )
            testthat::expect_s4_class(
                acbind(seed, seed),
                "BPCellsColBindMatrixSeed"
            )
            testthat::expect_equal(
                as.matrix(acbind(seed, seed)),
                cbind(mat, mat)
            )
            testthat::expect_s4_class(
                bindCOLS(seed, list(seed)),
                "BPCellsColBindMatrixSeed"
            )
            testthat::expect_equal(
                as.matrix(bindCOLS(seed, list(seed))),
                cbind(mat, mat)
            )
        }
    )

    testthat::test_that(
        sprintf("`cbind()` %s works as expected", matrix_name),
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


    testthat::test_that(
        sprintf("`+` %s works as expected", seed_name),
        {
            seed <- seed_fn(obj)
            testthat::expect_s4_class(seed, seed_class)
            testthat::expect_s4_class(seed + 1, "BPCellsTransformScaleShiftSeed")
            testthat::expect_equal(as.matrix(seed + 1), mat + 1)
            testthat::expect_s4_class(seed + 1 + 10, "BPCellsTransformScaleShiftSeed")
            testthat::expect_equal(as.matrix(seed + 1 + 10), mat + 1 + 10)
            testthat::expect_s4_class(
                seed + seq_len(nrow(seed)),
                "BPCellsTransformScaleShiftSeed"
            )
            testthat::expect_equal(
                as.matrix(seed + seq_len(nrow(seed))),
                mat + seq_len(nrow(seed))
            )
            testthat::expect_s4_class(
                t(seed) + seq_len(ncol(seed)),
                "BPCellsTransformScaleShiftSeed"
            )
            testthat::expect_equal(
                as.matrix(t(seed) + seq_len(ncol(seed))),
                t(mat) + seq_len(ncol(seed))
            )
            testthat::expect_s4_class(1 + seed, "BPCellsTransformScaleShiftSeed")
            testthat::expect_s4_class(
                seq_len(nrow(seed)) + seed,
                "BPCellsTransformScaleShiftSeed"
            )
            testthat::expect_equal(
                as.matrix(seq_len(nrow(seed)) + seed),
                seq_len(nrow(seed)) + mat
            )
            testthat::expect_s4_class(
                seq_len(ncol(seed)) + t(seed),
                "BPCellsTransformScaleShiftSeed"
            )
            testthat::expect_equal(
                as.matrix(seq_len(ncol(seed)) + t(seed)),
                seq_len(ncol(seed)) + t(mat)
            )
        }
    )

    testthat::test_that(
        sprintf("`+` %s works as expected", matrix_name),
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

    testthat::test_that(
        sprintf("`-` %s works as expected", seed_name),
        {
            seed <- seed_fn(obj)
            testthat::expect_s4_class(seed, seed_class)
            testthat::expect_s4_class(seed - 1, "BPCellsTransformScaleShiftSeed")
            testthat::expect_equal(as.matrix(seed - 1), mat - 1)
            testthat::expect_s4_class(seed - 1 - 10, "BPCellsTransformScaleShiftSeed")
            testthat::expect_equal(as.matrix(seed - 1 - 10), mat - 1 - 10)
            testthat::expect_s4_class(
                seed - seq_len(nrow(seed)),
                "BPCellsTransformScaleShiftSeed"
            )
            testthat::expect_equal(
                as.matrix(seed - seq_len(nrow(seed))),
                mat - seq_len(nrow(seed))
            )
            testthat::expect_s4_class(
                t(seed) - seq_len(ncol(seed)),
                "BPCellsTransformScaleShiftSeed"
            )
            testthat::expect_equal(
                as.matrix(t(seed) - seq_len(ncol(seed))),
                t(mat) - seq_len(ncol(seed))
            )
            testthat::expect_s4_class(1 - seed, "BPCellsTransformScaleShiftSeed")
            testthat::expect_s4_class(
                seq_len(nrow(seed)) - seed,
                "BPCellsTransformScaleShiftSeed"
            )
            testthat::expect_equal(
                as.matrix(seq_len(nrow(seed)) - seed),
                seq_len(nrow(seed)) - mat
            )
            testthat::expect_s4_class(
                seq_len(ncol(seed)) - t(seed),
                "BPCellsTransformScaleShiftSeed"
            )
            testthat::expect_equal(
                as.matrix(seq_len(ncol(seed)) - t(seed)),
                seq_len(ncol(seed)) - t(mat)
            )
        }
    )

    testthat::test_that(
        sprintf("`-` %s works as expected", matrix_name),
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


    testthat::test_that(
        sprintf("`*` %s works as expected", seed_name),
        {
            seed <- seed_fn(obj)
            testthat::expect_s4_class(seed, seed_class)
            testthat::expect_s4_class(seed * 1, "BPCellsTransformScaleShiftSeed")
            testthat::expect_equal(as.matrix(seed * 1), mat * 1)
            testthat::expect_s4_class(seed * 1 * 10, "BPCellsTransformScaleShiftSeed")
            testthat::expect_equal(as.matrix(seed * 1 * 10), mat * 1 * 10)
            testthat::expect_s4_class(
                seed * seq_len(nrow(seed)),
                "BPCellsTransformScaleShiftSeed"
            )
            testthat::expect_equal(
                as.matrix(seed * seq_len(nrow(seed))),
                mat * seq_len(nrow(seed))
            )
            testthat::expect_s4_class(
                t(seed) * seq_len(ncol(seed)),
                "BPCellsTransformScaleShiftSeed"
            )
            testthat::expect_equal(
                as.matrix(t(seed) * seq_len(ncol(seed))),
                t(mat) * seq_len(ncol(seed))
            )
            testthat::expect_s4_class(1 * seed, "BPCellsTransformScaleShiftSeed")
            testthat::expect_s4_class(
                seq_len(nrow(seed)) * seed,
                "BPCellsTransformScaleShiftSeed"
            )
            testthat::expect_equal(
                as.matrix(seq_len(nrow(seed)) * seed),
                seq_len(nrow(seed)) * mat
            )
            testthat::expect_s4_class(
                seq_len(ncol(seed)) * t(seed),
                "BPCellsTransformScaleShiftSeed"
            )
            testthat::expect_equal(
                as.matrix(seq_len(ncol(seed)) * t(seed)),
                seq_len(ncol(seed)) * t(mat)
            )
        }
    )

    testthat::test_that(
        sprintf("`*` %s works as expected", matrix_name),
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

    testthat::test_that(
        sprintf("`/` %s works as expected", seed_name),
        {
            seed <- seed_fn(obj)
            testthat::expect_s4_class(seed, seed_class)
            testthat::expect_s4_class(seed / 1, "BPCellsTransformScaleShiftSeed")
            testthat::expect_equal(as.matrix(seed / 1), mat / 1)
            testthat::expect_s4_class(seed / 1 / 10, "BPCellsTransformScaleShiftSeed")
            testthat::expect_equal(as.matrix(seed / 1 / 10), mat / 1 / 10)
            testthat::expect_s4_class(
                seed / seq_len(nrow(seed)),
                "BPCellsTransformScaleShiftSeed"
            )
            testthat::expect_equal(
                as.matrix(seed / seq_len(nrow(seed))),
                mat / seq_len(nrow(seed))
            )
            testthat::expect_s4_class(
                t(seed) / seq_len(ncol(seed)),
                "BPCellsTransformScaleShiftSeed"
            )
            testthat::expect_equal(
                as.matrix(t(seed) / seq_len(ncol(seed))),
                t(mat) / seq_len(ncol(seed))
            )
            testthat::expect_error(1 / seed)
        }
    )

    testthat::test_that(
        sprintf("`/` %s works as expected", matrix_name),
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
