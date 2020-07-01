test_that("make_filename works", {
    expect_equal(make_filename(2020), "accident_2020.csv.bz2")
    expect_equal(make_filename(2020.10), "accident_2020.csv.bz2")
    expect_equal(make_filename("2020"), "accident_2020.csv.bz2")
    expect_equal(make_filename(2021), "accident_2021.csv.bz2")
})
