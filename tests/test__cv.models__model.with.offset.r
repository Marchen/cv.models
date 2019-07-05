library(cv.models)
library(testthat)

#-----------------------------------------------------------------------------
#	Create test data.
#-----------------------------------------------------------------------------
create.test.data <- function() {
	set.seed(12345)
	test.data <- data.frame(
		x = rnorm(1000, 1000, 100),
		offset = runif(1000, 1, 10) %/% 1,
		random = runif(1000, 1, 100) %/% 1
	)
	test.data$y.norm <- rnorm(1000, test.data$x) * test.data$offset
	test.data$y.pois <- rpois(1000, test.data$x) * test.data$offset
	test.data$y.norm.with.random <- (
		rnorm(1000, test.data$x) * test.data$offset
		+ rnorm(100)[test.data$random]
	)
	test.data$y.pois.with.random <- (
		rpois(1000, test.data$x + rnorm(100)[test.data$random])
		* test.data$offset
	)
	test.data$random <- factor(test.data$random)
	return(test.data)
}

#-----------------------------------------------------------------------------
#	Test the model with offset (more correct model) can have
#	higher performance than the model without offset.
#-----------------------------------------------------------------------------
test_that(
	paste(
		"Test the model with offset having higher performance than the model",
		"without offset."
	), {
		# Run cv.models.
		d <- create.test.data()
		cv.no.offset <- cv.models(glm(y.pois ~ x, family = poisson, data = d))
		cv.offset <- cv.models(
			glm(y.pois ~ x + offset(log(offset)), family = poisson,	data = d),
		)
		# Check results.
		errors <- c(
			"mse", "rmse", "sd.mse", "sd.rmse", "sd.r.squared", "sd.spearman",
			"sd.kendall", "sd.q.squared"
		)
		cors <- c("r.squared", "spearman", "kendall", "q.squared")
		metrics.no.offset <- extract.metrics(cv.no.offset)
		metrics.offset <- extract.metrics(cv.no.offset)
		expect_true(all(metrics.no.offset[errors] >= metrics.offset[errors]))
		expect_true(all(metrics.no.offset[cors] <= metrics.offset[cors]))
	}
)
