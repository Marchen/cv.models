#==============================================================================
#	Unit test of regression.metrics.calculator class.
#==============================================================================
library(testthat)
library(cv.models)


#------------------------------------------------------------------------------
context("Unit test for regression.metrics.calculator class.")


#------------------------------------------------------------------------------
#	Create test data for 'fit'.
#
#	Expected metrics:
#		MSE = 0.1
#		RMSE = sqrt(0.1)
#		R^2 = 0.9929162
#		Q^2 = 0.9891775
#------------------------------------------------------------------------------
create.test.fit <- function() {
	fit <- list(
		response = 1:10, prediction = 1:10
	)
	fit$response[10] <- fit$response[10] + 1
	return(fit)
}


#------------------------------------------------------------------------------
test_that(
	"Test format of result returned by regression.metrics.calculator.", {
		# Prepare information for testing.
		fit <- create.test.fit()
		object <- cv.models:::regression.metrics.calculator$new()
		metrics <- object$calculate.metrics(fit)
		# Check data format.
		expect_is(metrics, "list", "Test the result is a list.")
		expect_true(
			all(sapply(metrics, is.matrix)),
			"Test all elements in the list are matrix."
		)
	}
)


#------------------------------------------------------------------------------
test_that(
	"Test values returned by regression.metrics.calculator.", {
	# Prepare information for testing.
		fit <- create.test.fit()
		object <- cv.models:::regression.metrics.calculator$new()
		metrics <- object$calculate.metrics(fit)
		# Check values.
		# Because expect_equal produces error because reference values don't
		# have names, following tests use expect_equivalent.
		expect_equivalent(
			metrics[[1]][, "mse"], 0.1, info = "Test value of MSE."
		)
		expect_equivalent(
			metrics[[1]][, "rmse"], sqrt(0.1), info = "Test value of RMSE."
		)
		expect_equivalent(
			metrics[[1]][, "r.squared"], cor(fit$response, fit$prediction) ^ 2,
			info = "Test value of R^2."
		)
		q2 = 1 - 1 / sum((fit$response - mean(fit$response)) ^ 2)
		expect_equivalent(
			metrics[[1]][, "q.squared"], q2, info = "Test value of Q^2."
		)
	}
)


rm(create.test.fit)
