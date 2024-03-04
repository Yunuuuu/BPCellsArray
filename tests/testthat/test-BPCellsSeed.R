m0 <- mock_matrix(30, 20)

testthat::test_that("BPCellsSeed() works well for dense matrix", {
    double_m <- m0
    # double dense matrix
    testthat::expect_identical(storage.mode(double_m), "double")
    m <- BPCellsSeed(double_m)
    testthat::expect_s4_class(m, "IterableMatrix")
    testthat::expect_identical(storage_mode(m), "double")
    # integer dense matrix
    integer_m <- m0
    storage.mode(integer_m) <- "integer"
    m <- BPCellsSeed(integer_m)
    testthat::expect_s4_class(m, "IterableMatrix")
    testthat::expect_identical(storage_mode(m), "uint32_t")
})

testthat::test_that("BPCellsSeed() works well for dgCMatrix", {
    sparce_m <- methods::as(m0, "dgCMatrix")
    m <- BPCellsSeed(sparce_m)
    testthat::expect_s4_class(m, "IterableMatrix")
    testthat::expect_identical(storage_mode(m), "double")
})

testthat::test_that("BPCellsSeed() works well for IterableMatrix", {
    iterable_mat <- methods::as(methods::as(m0, "dgCMatrix"), "IterableMatrix")
    m <- BPCellsSeed(iterable_mat)
    testthat::expect_identical(m, iterable_mat)
})

cli::test_that_cli("BPCellsSeed() works well for `ANY`", {
    testthat::expect_snapshot_error(BPCellsSeed(1))
    # execute BPCellsSeed in current package
    fn1 <- function(a) BPCellsSeed(a)
    environment(fn1) <- asNamespace(pkg_nm())
    testthat::expect_snapshot_error(fn1(1))
    # execute BPCellsSeed outside
    fn2 <- function(a) BPCellsSeed(a)
    testthat::expect_snapshot_error(fn2(1))
})

testthat::test_that("type() works well for `IterableMatrix`", {
    double_m <- methods::as(methods::as(m0, "dgCMatrix"), "IterableMatrix")
    testthat::expect_identical(type(double_m), "double")
    float_m <- BPCells::convert_matrix_type(double_m, "float")
    testthat::expect_identical(type(float_m), "double")
    uint32_t_m <- BPCells::convert_matrix_type(double_m, "uint32_t")
    testthat::expect_identical(type(uint32_t_m), "integer")
})

testthat::test_that(
    "as_matrix_IterableMatrix() works well for `IterableMatrix`",
    {
        m_double <- methods::as(
            methods::as(m0, "dgCMatrix"), "IterableMatrix"
        )
        # double IterableMatrix
        testthat::expect_identical(storage_mode(m_double), "double")
        testthat::expect_identical(
            storage.mode(as_matrix_IterableMatrix(m_double)), "double"
        )
        # float IterableMatrix
        m_float <- BPCells::convert_matrix_type(m_double, "float")
        testthat::expect_identical(storage_mode(m_float), "float")
        testthat::expect_identical(
            storage.mode(as_matrix_IterableMatrix(m_float)), "double"
        )
        # integer IterableMatrix
        m_integer <- BPCells::convert_matrix_type(m_double, "uint32_t")
        testthat::expect_identical(storage_mode(m_integer), "uint32_t")
        testthat::expect_identical(
            storage.mode(as_matrix_IterableMatrix(m_integer)), "integer"
        )
        # value exceed `.Machine$integer.max` return double mode and warn
        # message
        m_large <- m0
        m_large[1L] <- m_large[1L] + 1 + .Machine$integer.max
        m_large <- methods::as(
            methods::as(m_large, "dgCMatrix"), "IterableMatrix"
        )
        m_large_integer <- BPCells::convert_matrix_type(m_large, "uint32_t")
        testthat::expect_identical(storage_mode(m_large_integer), "uint32_t")
        testthat::expect_warning(
            dense_mat <- as_matrix_IterableMatrix(m_large_integer)
        )
        testthat::expect_identical(storage.mode(dense_mat), "double")
    }
)
