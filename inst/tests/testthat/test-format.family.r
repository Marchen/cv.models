require(testthat)


#-------------------------------------------------------------------------------
test_that("Test format.family", {
	# ������
	family <- format.family("binomial", type = "family")
	expect_equal(family, binomial())
	# �֐�
	family <- format.family(binomial, type = "family")
	expect_equal(family, binomial())
	# �I�u�W�F�N�g
	family <- format.family(binomial(), type = "family")
	expect_equal(family, binomial())

	# ������
	family <- format.family("binomial", type = "character")
	expect_equal(family, "binomial")
	# �֐�
	family <- format.family(binomial, type = "character")
	expect_equal(family, "binomial")
	# �I�u�W�F�N�g
	family <- format.family(binomial(), type = "character")
	expect_equal(family, "binomial")
})

