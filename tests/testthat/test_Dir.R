mat <- mock_matrix(30, 20)
path <- normalizePath(tempfile(tmpdir = tmpdir), mustWork = FALSE)
obj <- BPCells::write_matrix_dir(mat = as(mat, "dgCMatrix"), dir = path)

common_test(
    obj, path,
    mat = mat, 
    name = "Dir"
)
testthat::test_that("`subset()` BPCellsDirSeed object works as expected", {
    seed <- BPCellsSeed(obj)
    testthat::expect_s4_class(seed[1:10, ], "BPCellsSubsetSeed")
    testthat::expect_s4_class(seed[, 1:10], "BPCellsSubsetSeed")
    testthat::expect_s4_class(seed[1:10, 1:10], "BPCellsSubsetSeed")
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
