mat <- mock_matrix(30, 20)
path <- normalizePath(tempfile(tmpdir = tmpdir), mustWork = FALSE)
obj <- BPCells::write_matrix_dir(mat = as(mat, "dgCMatrix"), dir = path)

testthat::test_that(
    "`to_DelayedArray()` and `to_BPCells()` works well for `TransformScaleShift` object",
    {
        obj1 <- obj + 1
        # to_DelayedArray
        delayedop_obj <- to_DelayedArray(obj1)
        testthat::expect_s4_class(
            delayedop_obj,
            "BPCellsDelayedTransformScaleShift"
        )
        testthat::expect_identical(obj1@matrix, delayedop_obj@seed)
        # to_BPCells
        bpcells_obj <- to_BPCells(delayedop_obj)
        testthat::expect_s4_class(bpcells_obj, "TransformScaleShift")
        testthat::expect_identical(bpcells_obj, obj1)

        obj2 <- obj * 2
        # to_DelayedArray
        delayedop_obj <- to_DelayedArray(obj2)
        testthat::expect_s4_class(
            delayedop_obj,
            "BPCellsDelayedTransformScaleShift"
        )
        testthat::expect_identical(obj2@matrix, delayedop_obj@seed)
        # to_BPCells
        bpcells_obj <- to_BPCells(delayedop_obj)
        testthat::expect_s4_class(bpcells_obj, "TransformScaleShift")
        testthat::expect_identical(bpcells_obj, obj2)
    }
)

test_methods(obj + 1, mat = mat + 1, name = "TransformScaleShift")

test_methods(obj * 2, mat = mat * 2, name = "TransformScaleShift")
