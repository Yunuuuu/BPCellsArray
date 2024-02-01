mat <- mock_matrix(30, 20)
path <- normalizePath(tempfile(tmpdir = tmpdir), mustWork = FALSE)
obj <- BPCells::write_matrix_dir(mat = as(mat, "dgCMatrix"), dir = path)
mask <- matrix(
    sample(c(0, 1),
        size = nrow(obj) * ncol(obj), replace = TRUE,
        prob = c(0.7, 0.3)
    ),
    nrow = nrow(obj)
)
mat[mask > 0L] <- 0L
mask <- methods::as(mask, "dgCMatrix")
obj <- BPCells:::mask_matrix(obj, mask)

common_test(
    obj, path,
    mat = mat, 
    seed_fn = BPCellsMaskSeed, 
    name = "Mask"
)
testthat::test_that("`subset()` BPCellsMaskSeed object works as expected", {
    seed <- BPCellsMaskSeed(obj)
    testthat::expect_s4_class(seed[1:10, ], "BPCellsSubsetSeed")
    testthat::expect_s4_class(seed[, 1:10], "BPCellsSubsetSeed")
    testthat::expect_s4_class(seed[1:10, 1:10], "BPCellsSubsetSeed")
})
