mat1 <- mock_matrix(200, 200)
mat2 <- mock_matrix(ncol(mat1), 100)
path <- normalizePath(
    c(tempfile(tmpdir = tmpdir), tempfile(tmpdir = tmpdir)),
    mustWork = FALSE
)
obj <- BPCells::write_matrix_dir(mat = as(mat1, "dgCMatrix"), dir = path[1L])
obj2 <- BPCells::write_matrix_dir(mat = as(mat2, "dgCMatrix"), dir = path[2L])
obj <- obj %*% obj2
mat <- mat1 %*% mat2

common_test(mat, obj, path, BPCellsMultiplySeed, "Multiply")
testthat::test_that("`subset()` BPCellsMultiplySeed object works as expected", {
    seed <- BPCellsMultiplySeed(obj)
    testthat::expect_s4_class(seed[1:10, ], "BPCellsMultiplySeed")
    testthat::expect_s4_class(seed[, 1:10], "BPCellsMultiplySeed")
    testthat::expect_s4_class(seed[1:10, 1:10], "BPCellsMultiplySeed")
})
