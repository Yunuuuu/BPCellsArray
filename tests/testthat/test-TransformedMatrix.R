mat <- mock_matrix(30, 20)
path <- normalizePath(tempfile(tmpdir = tmpdir), mustWork = FALSE)
obj <- BPCells::write_matrix_dir(mat = as(mat, "dgCMatrix"), dir = path)

test_methods(
    BPCells::expm1_slow(obj), path,
    name = "TransformExpm1Slow",
    skip_multiplication = TRUE # often return Inf value
)

test_methods(
    BPCells::log1p_slow(obj), path,
    name = "TransformLog1p"
)

# test_methods(
#     expm1(obj), path,
#     name = "TransformExpm1",
#     skip_multiplication = TRUE # often return Inf value
# )

# test_methods(
#     log1p(obj), path,
#     name = "TransformLog1pSingle"
# )

test_methods(
    obj + 1, path,
    mat = mat + 1,
    name = "TransformScaleShift"
)

test_methods(
    obj * 2, path,
    mat = mat * 2,
    name = "TransformScaleShift"
)
