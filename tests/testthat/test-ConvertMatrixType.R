mat <- mock_matrix(30, 20)
path <- normalizePath(tempfile(tmpdir = tmpdir), mustWork = FALSE)
obj <- BPCells::write_matrix_dir(mat = as(mat, "dgCMatrix"), dir = path)
obj <- BPCells::convert_matrix_type(obj, "uint32_t")
mat <- matrix_to_integer(mat)

testthat::test_that("`to_DelayedArray()` and `to_BPCells()` works well for `ConvertMatrixType` object", {
    # to_DelayedArray
    delayedop_obj <- to_DelayedArray(obj)
    testthat::expect_s4_class(delayedop_obj, "BPCellsDelayedConvert")
    testthat::expect_identical(obj@matrix, delayedop_obj@seed)
    # to_BPCells
    bpcells_obj <- to_BPCells(delayedop_obj)
    testthat::expect_s4_class(bpcells_obj, "ConvertMatrixType")
    testthat::expect_identical(bpcells_obj, obj)
})

test_methods(obj, mode = "uint32_t", mat = mat, name = "ConvertMatrixType")
