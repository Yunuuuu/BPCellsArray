mat <- mock_matrix(2000, 200)
path <- tempfile()
obj <- BPCells::write_matrix_dir(mat = as(mat, "dgCMatrix"), dir = path)
obj <- obj[1:1000L, 1:100]

testthat::test_that("`BPCellsSubsetSeed()` works as expected", {
    seed <- BPCellsSubsetSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsSubsetSeed")
    obj <- BPCellsSubsetArray(seed)
    testthat::expect_s4_class(obj, "BPCellsSubsetMatrix")
    testthat::expect_identical(path(seed), path)
    testthat::expect_identical(path(obj), path)
})

testthat::test_that("subset `BPCellsSubsetSeed` object works as expected", {
    seed <- BPCellsSubsetSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsSubsetSeed")
    testthat::expect_identical(path(seed), path)
    testthat::expect_s4_class(seed[1:10, 1:10], "BPCellsSubsetSeed")
})

testthat::test_that("subset `BPCellsSubsetMatrix` object works as expected", {
    obj <- BPCellsSubsetArray(obj)
    testthat::expect_s4_class(obj, "BPCellsSubsetMatrix")
    testthat::expect_identical(path(obj), path)
    testthat::expect_s4_class(obj[1:10, 1:10], "BPCellsSubsetMatrix")
})

testthat::test_that("`convert_type` for `BPCellsSubsetSeed` object works as expected", {
    seed <- BPCellsSubsetSeed(obj)
    float_seed <- convert_type(seed, "numeric")
    testthat::expect_s4_class(float_seed, "BPCellsConvertSeed")
    testthat::expect_identical(type(float_seed), "double")
    integer_seed <- convert_type(float_seed, "integer")
    testthat::expect_s4_class(integer_seed, "BPCellsConvertSeed")
    testthat::expect_identical(type(integer_seed), "integer")
})

testthat::test_that("`convert_type` for `BPCellsSubsetMatrix` object works as expected", {
    obj <- BPCellsSubsetArray(obj)
    float_obj <- convert_type(obj, "numeric")
    testthat::expect_s4_class(float_obj, "BPCellsConvertMatrix")
    testthat::expect_identical(type(float_obj), "double")
    integer_obj <- convert_type(float_obj, "integer")
    testthat::expect_s4_class(integer_obj, "BPCellsConvertMatrix")
    testthat::expect_identical(type(integer_obj), "integer")
})

testthat::test_that("`t()` for `BPCellsSubset` object works as expected", {
    seed <- BPCellsSubsetSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsSubsetSeed")
    testthat::expect_s4_class(t(seed), "BPCellsSubsetSeed")
    obj <- BPCellsSubsetArray(obj)
    testthat::expect_s4_class(t(obj), "BPCellsSubsetMatrix")
})

testthat::test_that("`dimnames<-` for `BPCellsSubset` object works as expected", {
    seed <- BPCellsSubsetSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsSubsetSeed")
    dimnames(seed) <- list(
        paste0("G", seq_len(nrow(seed))),
        paste0("C", seq_len(ncol(seed)))
    )
    testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
    obj <- BPCellsSubsetArray(obj)
    testthat::expect_s4_class(obj, "BPCellsSubsetMatrix")
    dimnames(obj) <- list(
        paste0("G", seq_len(nrow(obj))),
        paste0("C", seq_len(ncol(obj)))
    )
    testthat::expect_s4_class(obj, "BPCellsRenameDimsMatrix")
})

testthat::test_that("`%*%` for `BPCellsSubset` object works as expected", {
    seed <- BPCellsSubsetSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsSubsetSeed")
    testthat::expect_s4_class(seed %*% t(seed), "BPCellsMultiplySeed")
    testthat::expect_true(is.matrix(seed %*% as.matrix(t(seed))))
    testthat::expect_true(is.matrix(seed %*% seq_len(ncol(seed))))
    testthat::expect_true(is.matrix(seq_len(nrow(seed)) %*% seed))
    obj <- BPCellsSubsetArray(obj)
    testthat::expect_s4_class(obj, "BPCellsSubsetMatrix")
    testthat::expect_s4_class(obj %*% t(obj), "BPCellsMultiplyMatrix")
    testthat::expect_true(is.matrix(obj %*% as.matrix(t(obj))))
    testthat::expect_true(is.matrix(obj %*% seq_len(ncol(obj))))
    testthat::expect_true(is.matrix(seq_len(nrow(obj)) %*% obj))
})
