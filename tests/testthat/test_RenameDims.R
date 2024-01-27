mat <- mock_matrix(2000, 200)
path <- normalizePath(tempfile(tmpdir = tmpdir), mustWork = FALSE)
obj <- BPCells::write_matrix_dir(mat = as(mat, "dgCMatrix"), dir = path)
dimnames(obj) <- list(
    paste0("G", seq_len(2000L)),
    paste0("C", seq_len(200))
)
dimnames(mat) <- list(
    paste0("G", seq_len(2000L)),
    paste0("C", seq_len(200))
)

common_test(mat, obj, path, BPCellsRenameDimsSeed, "RenameDims")
testthat::test_that(
    "`subset()` BPCellsRenameDimsSeed object works as expected",
    {
        seed <- BPCellsRenameDimsSeed(obj)
        testthat::expect_s4_class(seed[1:10, ], "BPCellsRenameDimsSeed")
        testthat::expect_s4_class(seed[, 1:10], "BPCellsRenameDimsSeed")
        testthat::expect_s4_class(seed[1:10, 1:10], "BPCellsRenameDimsSeed")
    }
)
