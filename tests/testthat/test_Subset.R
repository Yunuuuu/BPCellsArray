mat <- mock_matrix(200, 40)
path <- normalizePath(tempfile(tmpdir = tmpdir), mustWork = FALSE)
obj <- BPCells::write_matrix_dir(mat = as(mat, "dgCMatrix"), dir = path)
obj <- obj[1:100L, 1:20]
mat <- mat[1:100L, 1:20]

common_test(
    obj, path,
    mat = mat, 
    seed_fn = BPCellsSubsetSeed, 
    name = "Subset"
)
testthat::test_that("`subset()` BPCellsSubsetSeed object works as expected", {
    seed <- BPCellsSubsetSeed(obj)
    testthat::expect_s4_class(seed[1:10, ], "BPCellsSubsetSeed")
    testthat::expect_s4_class(seed[, 1:10], "BPCellsSubsetSeed")
    testthat::expect_s4_class(seed[1:10, 1:10], "BPCellsSubsetSeed")
})
