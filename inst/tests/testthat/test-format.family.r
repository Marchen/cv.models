require(testthat)


#-------------------------------------------------------------------------------
test_that("Test format.family", {
	# 文字列
	family <- format.family("binomial", type = "family")
	expect_equal(family, binomial())
	# 関数
	family <- format.family(binomial, type = "family")
	expect_equal(family, binomial())
	# オブジェクト
	family <- format.family(binomial(), type = "family")
	expect_equal(family, binomial())

	# 文字列
	family <- format.family("binomial", type = "character")
	expect_equal(family, "binomial")
	# 関数
	family <- format.family(binomial, type = "character")
	expect_equal(family, "binomial")
	# オブジェクト
	family <- format.family(binomial(), type = "character")
	expect_equal(family, "binomial")
})

