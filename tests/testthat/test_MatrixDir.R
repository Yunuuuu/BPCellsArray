mat <- mock_matrix(2000, 200)
path <- tempfile()
obj <- BPCells::write_matrix_dir(mat = as(mat, "dgCMatrix"), dir = path)

testthat::test_that("`BPCellsDirSeed()` works as expected", {
    seed <- BPCellsDirSeed(path)
    testthat::expect_s4_class(seed, "BPCellsDirSeed")
    seed <- BPCellsDirSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsDirSeed")
    obj <- DelayedArray(seed)
    testthat::expect_s4_class(obj, "BPCellsDirMatrix")
    testthat::expect_identical(path(seed), path)
    testthat::expect_identical(path(obj), path)
})

testthat::test_that("`writeBPCellsDirArray()` works as expected", {
    testthat::expect_no_error(
        obj <- writeBPCellsDirArray(mat, path = path, overwrite = TRUE)
    )
    testthat::expect_identical(path(obj), path)
    testthat::expect_s4_class(obj, "BPCellsDirMatrix")
})

testthat::test_that("`as()` methods works as expected", {
    testthat::expect_s4_class(as(mat, "BPCellsDirMatrix"), "BPCellsDirMatrix")
    testthat::expect_s4_class(as(mat, "BPCellsDirArray"), "BPCellsDirMatrix")
})

testthat::test_that("subset `BPCellsDirSeed` object works as expected", {
    seed <- BPCellsDirSeed(path)
    testthat::expect_s4_class(seed[1:10, 1:10], "BPCellsSubsetSeed")
})

testthat::test_that("subset `BPCellsDirMatrix` object works as expected", {
    obj <- BPCellsDirArray(path)
    testthat::expect_s4_class(obj, "BPCellsDirMatrix")
    testthat::expect_identical(path(obj), path)
    testthat::expect_s4_class(obj[1:10, 1:10], "BPCellsSubsetMatrix")
})

testthat::test_that("`convert_type` for `BPCellsDirSeed` object works as expected", {
    seed <- BPCellsDirSeed(path)
    float_seed <- convert_type(seed, "numeric")
    testthat::expect_s4_class(float_seed, "BPCellsConvertSeed")
    testthat::expect_identical(type(float_seed), "double")
    integer_seed <- convert_type(float_seed, "integer")
    testthat::expect_s4_class(integer_seed, "BPCellsConvertSeed")
    testthat::expect_identical(type(integer_seed), "integer")
})

testthat::test_that("`convert_type` for `BPCellsDirMatrix` object works as expected", {
    obj <- BPCellsDirArray(path)
    float_obj <- convert_type(obj, "numeric")
    testthat::expect_s4_class(float_obj, "BPCellsConvertMatrix")
    testthat::expect_identical(type(float_obj), "double")
    integer_obj <- convert_type(float_obj, "integer")
    testthat::expect_s4_class(integer_obj, "BPCellsConvertMatrix")
    testthat::expect_identical(type(integer_obj), "integer")
})
