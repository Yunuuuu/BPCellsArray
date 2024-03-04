testthat::test_that("`with_seedform()` works well", {
    old_seedform <- get_seedform()
    testthat::expect_identical(
        with_seedform("DelayedArray", seedform()), "DelayedArray"
    )
    testthat::expect_identical(seedform(), old_seedform)
})

testthat::test_that("`seedform()` works well for string argument", {
    # BPCells
    old_seedform <- get_seedform()
    testthat::expect_identical(seedform("BPCells"), old_seedform)
    testthat::expect_identical(get_seedform(), "BPCells")

    # DelayedArray
    old_seedform <- get_seedform()
    testthat::expect_identical(seedform("DelayedArray"), old_seedform)
    testthat::expect_identical(get_seedform(), "DelayedArray")

    # error for un-supported seedform
    testthat::expect_error(seedform("UnSupportedSeedForm"))
    # error for multiple items
    testthat::expect_error(seedform(letters))
})
