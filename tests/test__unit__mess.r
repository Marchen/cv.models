#==============================================================================
#	Unit test of MESS calculation
#==============================================================================
library(testthat)
library(cv.models)


#------------------------------------------------------------------------------
context("Unit test for MESS calculation")

#------------------------------------------------------------------------------
test_that(
	"Test correctness of MESS using single variable",
	{
		data.train <- data.frame(a = 0:100)
		data.test <- data.frame(
			a = c(-50, -1, 0, 1, 50, 51, 52, 100, 101, 102, 150)
		)
		expected <- c(
			# if fi = 0: (pi - mini) / (maxi - mini) * 100
			(-50 - 0) / (100 - 0) * 100,
			(-1 - 0) / (100 - 0) * 100,
			(0 - 0) / (100 - 0) * 100,
			# if 0 < fi ≤ 50: 2 * fi
			2 * 1 / length(0:100) * 100,
			2 * 50 / length(0:100) * 100,
			# if 50 ≤ fi < 100: 2 * (100 - fi)
			2 * (100 - (51 / length(0:100)) * 100),
			2 * (100 - (52 / length(0:100)) * 100),
			2 * (100 - (100 / length(0:100)) * 100),
			# if fi = 100: (maxi - pi) / (maxi - mini) * 100
			(100 - 101) / (100 - 0) * 100,
			(100 - 102) / (100 - 0) * 100,
			(100 - 150) / (100 - 0) * 100
		)
		result <- cv.models:::calculate.mess(data.train, data.test, TRUE)
		expect_equal(result, expected)
	}
)

#------------------------------------------------------------------------------
test_that(
	"Test MESS using multiple variables can select lowest values",
	{
		data.train <- data.frame(a = 0:100, b = 0:100)
		data.test <- data.frame(a = c(-50, 1, 50), b = c(-1, 0, 51))
		expected <- c(
			(-50 - 0) / (100 - 0) * 100,
			(0 - 0) / (100 - 0) * 100,
			2 * 50 / length(0:100) * 100
		)
		result <- cv.models:::calculate.mess(data.train, data.test, TRUE)
		expect_equal(result, expected)
	}
)
