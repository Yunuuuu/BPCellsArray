# Creating temporary directory
tmpdir <- testthat::test_path("_TEMP")
if (!dir.exists(tmpdir)) dir.create(tmpdir)
tmpdir <- normalizePath(tmpdir, mustWork = TRUE)

common_test <- function(
    obj, actual_path, ..., mat = NULL,
    seed_fn, name, 
    skip_multiplication = FALSE) {
    # for TransformedMatrix, it often contain float values
    transformed <- methods::is(seed_fn(obj), "BPCellsTransformedSeed")
    seed_class <- sprintf("BPCells%sSeed", name)
    seed_name <- sprintf("BPCellsSeed class of `%s`", name)
    matrix_name <- sprintf("BPCellsMatrix class of `%s`", name)
    mat <- mat %||% as.matrix(obj)
    cli::cli_inform("{.field BPCells{name}Seed} works as expected")
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

    cli::cli_inform("{.field subset} {seed_name} works as expected")
    testthat::test_that(
        sprintf("`subset()` %s works as expected", seed_name),
        {
            seed <- seed_fn(obj)
            testthat::expect_s4_class(seed[1:10, ], "BPCellsSeed")
            testthat::expect_identical(
                rownames(seed[1:10, ]),
                rownames(seed)[1:10]
            )
            testthat::expect_identical(colnames(seed[1:10, ]), colnames(seed))
            testthat::expect_equal(as.matrix(seed[1:10, ]), mat[1:10, ])
            testthat::expect_s4_class(seed[, 1:10], "BPCellsSeed")
            testthat::expect_identical(
                colnames(seed[, 1:10]),
                colnames(seed)[1:10]
            )
            testthat::expect_identical(rownames(seed[, 1:10]), rownames(seed))
            testthat::expect_equal(as.matrix(seed[, 1:10]), mat[, 1:10])
            seed2 <- seed[1:10, 1:10]
            testthat::expect_s4_class(seed2, "BPCellsSeed")
            testthat::expect_identical(rownames(seed2), rownames(seed)[1:10])
            testthat::expect_identical(colnames(seed2), colnames(seed)[1:10])
            testthat::expect_equal(as.matrix(seed2), mat[1:10, 1:10])
        }
    )


    cli::cli_inform("{.field subset} {matrix_name} works as expected")
    testthat::test_that(
        sprintf("`subset()` %s works as expected", matrix_name),
        {
            obj <- BPCellsArray(obj)
            testthat::expect_s4_class(obj[1:10, ], "BPCellsMatrix")
            testthat::expect_identical(
                rownames(obj[1:10, ]),
                rownames(obj)[1:10]
            )
            testthat::expect_identical(colnames(obj[1:10, ]), colnames(obj))
            testthat::expect_s4_class(obj[, 1:10], "BPCellsMatrix")
            testthat::expect_identical(
                colnames(obj[, 1:10]),
                colnames(obj)[1:10]
            )
            testthat::expect_identical(rownames(obj[, 1:10]), rownames(obj))
            obj2 <- obj[1:10, 1:10]
            testthat::expect_s4_class(obj2, "BPCellsMatrix")
            testthat::expect_identical(rownames(obj2), rownames(obj)[1:10])
            testthat::expect_identical(colnames(obj2), colnames(obj)[1:10])

            testthat::skip_if(transformed)
            testthat::expect_equal(as.matrix(obj[1:10, ]), mat[1:10, ])
            testthat::expect_equal(as.matrix(obj[, 1:10]), mat[, 1:10])
            testthat::expect_equal(as.matrix(obj2), mat[1:10, 1:10])
        }
    )

    pseudo_mat <- matrix(sample(mat, length(mat)), nrow = nrow(mat))
    cli::cli_inform("{.field [<-} {seed_name} works as expected")
    testthat::test_that(
        sprintf("`[<-()` %s works as expected", seed_name),
        {
            seed <- seed_fn(obj)
            seed[1:10, ] <- pseudo_mat[1:10, ]
            mat[1:10, ] <- pseudo_mat[1:10, ]
            testthat::expect_s4_class(seed, "BPCellsSeed")
            testthat::expect_equal(as.matrix(seed), mat)

            seed[, 1:10] <- pseudo_mat[, 1:10]
            mat[, 1:10] <- pseudo_mat[, 1:10]
            testthat::expect_s4_class(seed, "BPCellsSeed")
            testthat::expect_equal(as.matrix(seed), mat)

            seed[1:10, 1:10] <- pseudo_mat[1:10, 1:10]
            mat[1:10, 1:10] <- pseudo_mat[1:10, 1:10]
            testthat::expect_s4_class(seed, "BPCellsSeed")
            testthat::expect_equal(as.matrix(seed), mat)

            testthat::expect_identical(storage_mode(seed), "double")

            seed[1:10, mode = "integer"] <- pseudo_mat[1:10, ]
            mat[1:10, ] <- pseudo_mat[1:10, ]
            storage.mode(mat) <- "integer"
            testthat::expect_s4_class(seed, "BPCellsSeed")
            testthat::expect_identical(storage_mode(seed), "uint32_t")
            testthat::expect_equal(as.matrix(seed), mat)
        }
    )
    cli::cli_inform("{.field [<-} {matrix_name} works as expected")
    testthat::test_that(
        sprintf("`[<-()` %s works as expected", matrix_name),
        {
            obj <- BPCellsArray(obj)
            obj[1:10, ] <- pseudo_mat[1:10, ]
            mat[1:10, ] <- pseudo_mat[1:10, ]
            testthat::expect_s4_class(obj, "BPCellsMatrix")
            testthat::expect_equal(as.matrix(obj), mat)

            obj[, 1:10] <- pseudo_mat[, 1:10]
            mat[, 1:10] <- pseudo_mat[, 1:10]
            testthat::expect_s4_class(obj, "BPCellsMatrix")
            testthat::expect_equal(as.matrix(obj), mat)

            obj[1:10, 1:10] <- pseudo_mat[1:10, 1:10]
            mat[1:10, 1:10] <- pseudo_mat[1:10, 1:10]
            testthat::expect_s4_class(obj, "BPCellsMatrix")
            testthat::expect_equal(as.matrix(obj), mat)
        }
    )

    cli::cli_inform("{.field convert_mode} {seed_name} works as expected")
    testthat::test_that(
        sprintf("`convert_mode()` %s works as expected", seed_name),
        {
            seed <- seed_fn(obj)
            float_seed <- convert_mode(seed, "float")
            testthat::expect_s4_class(float_seed, "BPCellsConvertSeed")
            testthat::expect_identical(storage_mode(float_seed), "float")
            integer_seed <- convert_mode(float_seed, "uint32_t")
            testthat::expect_s4_class(integer_seed, "BPCellsConvertSeed")
            testthat::expect_identical(storage_mode(integer_seed), "uint32_t")
        }
    )

    cli::cli_inform("{.field convert_mode} {matrix_name} works as expected")
    testthat::test_that(
        sprintf("`convert_mode()` %s works as expected", matrix_name),
        {
            obj <- BPCellsArray(obj)
            float_obj <- convert_mode(obj, "float")
            testthat::expect_s4_class(float_obj, "BPCellsMatrix")
            testthat::expect_identical(storage_mode(float_obj), "float")
            integer_obj <- convert_mode(float_obj, "uint32_t")
            testthat::expect_s4_class(integer_obj, "BPCellsMatrix")
            testthat::expect_identical(storage_mode(integer_obj), "uint32_t")
        }
    )

    cli::cli_inform("{.field t} BPCells{name} works as expected")
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

    cli::cli_inform("{.field dimnames<-} BPCells{name} works as expected")
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

    cli::cli_inform("{.field rownames<-} BPCells{name} works as expected")
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

    cli::cli_inform("{.field colnames<-} BPCells{name} works as expected")
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
    cli::cli_inform("{.field %*%} BPCells{name} works as expected")
    testthat::test_that(
        sprintf("`%%*%%` BPCells%s works as expected", name),
        {
            testthat::skip_if(
                skip_multiplication,
                "Skipping for TransformedMatrix"
            )
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

    cli::cli_inform("{.field rbind} {seed_name} works as expected")
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

    cli::cli_inform("{.field rbind} {matrix_name} works as expected")
    testthat::test_that(
        sprintf("`rbind()` %s works as expected", matrix_name),
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
            testthat::skip_if(transformed)
            testthat::expect_equal(as.matrix(rbind2(obj, obj)), rbind(mat, mat))
            testthat::expect_equal(as.matrix(rbind(obj, obj)), rbind(mat, mat))
            testthat::expect_equal(as.matrix(arbind(obj, obj)), rbind(mat, mat))
            testthat::expect_equal(
                as.matrix(bindROWS(obj, list(obj))),
                rbind(mat, mat)
            )
        }
    )

    cli::cli_inform("{.field cbind} {seed_name} works as expected")
    testthat::test_that(
        sprintf("`cbind()` %s works as expected", seed_name),
        {
            seed <- seed_fn(obj)
            testthat::expect_s4_class(seed, seed_class)
            testthat::expect_s4_class(
                cbind2(seed, seed),
                "BPCellsColBindMatrixSeed"
            )
            testthat::expect_s4_class(
                cbind(seed, seed),
                "BPCellsColBindMatrixSeed"
            )
            testthat::expect_s4_class(
                acbind(seed, seed),
                "BPCellsColBindMatrixSeed"
            )
            testthat::expect_s4_class(
                bindCOLS(seed, list(seed)),
                "BPCellsColBindMatrixSeed"
            )
            testthat::skip_if(transformed)
            testthat::expect_equal(
                as.matrix(cbind2(seed, seed)),
                cbind(mat, mat)
            )
            testthat::expect_equal(
                as.matrix(cbind(seed, seed)),
                cbind(mat, mat)
            )
            testthat::expect_equal(
                as.matrix(acbind(seed, seed)),
                cbind(mat, mat)
            )
            testthat::expect_equal(
                as.matrix(bindCOLS(seed, list(seed))),
                cbind(mat, mat)
            )
        }
    )

    cli::cli_inform("{.field cbind} {matrix_name} works as expected")
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

    cli::cli_inform("{.field +} {seed_name} works as expected")
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

    cli::cli_inform("{.field +} {matrix_name} works as expected")
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

    cli::cli_inform("{.field -} {seed_name} works as expected")
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

    cli::cli_inform("{.field -} {matrix_name} works as expected")
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

    cli::cli_inform("{.field *} {seed_name} works as expected")
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

    cli::cli_inform("{.field *} {matrix_name} works as expected")
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

    cli::cli_inform("{.field /} {seed_name} works as expected")
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

    cli::cli_inform("{.field /} {matrix_name} works as expected")
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
