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
