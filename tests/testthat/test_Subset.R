mat <- mock_matrix(2000, 200)
path <- normalizePath(tempfile(tmpdir = tmpdir), mustWork = FALSE)
obj <- BPCells::write_matrix_dir(mat = as(mat, "dgCMatrix"), dir = path)
obj <- obj[1:1000L, 1:100]
mat <- mat[1:1000L, 1:100]

common_test(mat, obj, path, BPCellsSubsetSeed, "Subset")
testthat::test_that("`subset()` BPCellsSubsetSeed object works as expected", {
    seed <- BPCellsSubsetSeed(obj)
    testthat::expect_s4_class(seed[1:10, ], "BPCellsSubsetSeed")
    testthat::expect_s4_class(seed[, 1:10], "BPCellsSubsetSeed")
    testthat::expect_s4_class(seed[1:10, 1:10], "BPCellsSubsetSeed")
})
