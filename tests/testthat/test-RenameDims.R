mat <- mock_matrix(30, 20)
path <- normalizePath(tempfile(tmpdir = tmpdir), mustWork = FALSE)
obj <- BPCells::write_matrix_dir(mat = as(mat, "dgCMatrix"), dir = path)
dimnames(obj) <- list(
    paste0("G", seq_len(nrow(obj))),
    paste0("C", seq_len(ncol(obj)))
)
dimnames(mat) <- list(
    paste0("G", seq_len(nrow(obj))),
    paste0("C", seq_len(ncol(obj)))
)

testthat::test_that("`to_DelayedArray()` and `to_BPCells()` works well for `RenameDims` object", {
    # to_DelayedArray
    delayedop_obj <- to_DelayedArray(obj)
    testthat::expect_s4_class(delayedop_obj, "BPCellsDelayedRenameDims")
    testthat::expect_identical(obj@matrix, delayedop_obj@seed)
    # to_BPCells
    bpcells_obj <- to_BPCells(delayedop_obj)
    testthat::expect_s4_class(bpcells_obj, "RenameDims")
    testthat::expect_identical(bpcells_obj, obj)
})

test_methods(obj, mat = mat, name = "RenameDims")
