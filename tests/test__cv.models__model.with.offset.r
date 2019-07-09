library(cv.models)
library(testthat)
library(gbm)

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
test.offset <- function(call.with.offset, call.without.offset, ...) {
	test_that(
		paste(
			"Test the model with offset having higher performance than the",
			"model without offset."
		), {
			# Run cv.models.
			cv.no.offset <- cv.models(call.without.offset, ...)
			cv.offset <- cv.models(call.with.offset, ...)
			# Check results.
			errors <- c(
				"mse", "rmse", "sd.mse", "sd.rmse", "sd.r.squared",
				"sd.spearman", "sd.kendall", "sd.q.squared"
			)
			cors <- c("r.squared", "spearman", "kendall", "q.squared")
			metrics.no.offset <- extract.metrics(cv.no.offset)
			metrics.offset <- extract.metrics(cv.no.offset)
			expect_true(
				all(metrics.no.offset[errors] >= metrics.offset[errors])
			)
			expect_true(
				all(metrics.no.offset[cors] <= metrics.offset[cors])
			)
		}
	)
}

#-----------------------------------------------------------------------------
#	Test runner.
#
#	Currently, glm and gbm were tested.
#-----------------------------------------------------------------------------
run.test <- function() {
	d <- create.test.data()
	test.data <- list(
		glm = list(
			offset = substitute(
				glm(
					y.pois ~ x + offset(log(offset)), family = poisson,
					data = d
				)
			),
			no.offset = substitute(
				glm(y.pois ~ x, family = poisson, data = d)
			)
		),
		gbm = list(
			offset = substitute(
				gbm(
					y.pois ~ x + offset(log(offset)), n.cores = 1,
					distribution = "poisson", n.trees = 100, data = d
				)
			),
			no.offset = substitute(
				gbm(
					y.pois ~ x, distribution = "poisson", n.trees = 100,
					data = d, n.cores = 1
				)
			)
		)
	)
	for (i in names(test.data)) {
		if (i == "gbm") {
			test.offset(
				test.data[[i]]$offset, test.data[[i]]$no.offset, n.trees = 100
			)
		} else {
			test.offset(test.data[[i]]$offset, test.data[[i]]$no.offset)
		}
	}
}

run.test()
