#===============================================================================
#	Test setting same 'seed' produces same result.
#===============================================================================
library(testthat)
library(cv.models)
library(randomForest)
library(gbm)


#------------------------------------------------------------------------------
context("Preparing tests")

# Because R CMD CHECK
exceeds.core.limit <- function(n.cores) {
	is.limited <- Sys.getenv("_R_CHECK_LIMIT_CORES_", unset = "")
	return(ifelse(is.limited == "" | is.limited == "FALSE", FALSE, TRUE))
}

run.tests <- function(fun) {
	# Make calls of functions.
	call.glm <- substitute(glm(Petal.Length ~ ., data = iris))
	call.randomForest <- substitute(
		randomForest(Petal.Length ~ ., data = iris)
	)
	call.gbm <- substitute(
		gbm(
			Petal.Length ~ ., data = iris, weights = Sepal.Width,
			distribution = "gaussian", n.trees = 10, n.cores = 1
		)
	)
	# Run tests.
	fun(call.glm, "Testing glm.")
	fun(call.randomForest, "Testing randomForest.")
	fun(call.gbm, "Testing gbm.")
}


#------------------------------------------------------------------------------
context("Test same seeds produce same results")

test.same.seeds.produce.same.results <- function(call, msg) {
	test_that(
		msg, {
			cv.1 <- cv.models(call, seed = 1, n.cores = 1, n.trees = 10)
			cv.2 <- cv.models(call, seed = 1, n.cores = 1, n.trees = 10)
			fields <- names(cv.1)[!names(cv.1) %in% c("n.cores", "adapter")]
			for (i in fields) {
				expect_identical(
					cv.1[[i]], cv.2[[i]], info = sprintf("Field: %s", i)
				)
			}
		}
	)
}

run.tests(test.same.seeds.produce.same.results)


#------------------------------------------------------------------------------
context("Test different seeds produce different results")

test.different.seeds.produce.different.results <- function(call, msg) {
	test_that(
		msg, {
			cv.1 <- cv.models(call, seed = 1, n.cores = 1, n.trees = 10)
			cv.2 <- cv.models(call, seed = 2, n.cores = 1, n.trees = 10)
			fields <- names(cv.1)[!names(cv.1) %in% c("n.cores", "adapter")]
			expect_false(identical(cv.1$cv.results, cv.2$cv.results))
		}
	)
}

run.tests(test.different.seeds.produce.different.results)


#------------------------------------------------------------------------------
context("Test same seed produce same result with parameter grid")

test_that(
	"Test same seed produce same result with parameter grid.", {
		call.cv.models <- call(
			"cv.models",
			gbm(
				Petal.Length ~ ., data = iris, distribution = "gaussian",
				n.cores = 1, n.trees = 5
			),
			seed = 1, n.cores = 1,
			grid = list(
				interaction.depth = c(1, 5), n.minobsinnode = c(1, 10)
			),
			grid.predict = list(n.trees = 1:3)
		)
		cv.1 <- eval(call.cv.models)
		cv.2 <- eval(call.cv.models)
		expect_equal(cv.1$cv.results, cv.2$cv.results)
	}
)


#------------------------------------------------------------------------------
context("Test same seeds produce same results using cluster")

test.same.seeds.produce.same.results.with.custer <- function(call, msg) {
	test_that(
		msg, {
			# No cluster.
			cv.1 <- cv.models(
				call, seed = 1, folds = 2, n.cores = 1, n.trees = 10
			)
			# Using 2 workwers
			cv.2 <- cv.models(
				call, seed = 1, folds = 2, n.cores = 2, n.trees = 10
			)
			# Using 2/3 of workers
			if (!exceeds.core.limit(3)) {
				# Travis CI doesn't support >2 cores so skip on Travis.
				cv.3 <- cv.models(
					call, seed = 1, folds = 2, n.cores = 3, n.trees = 10
				)
			}
			fields <- names(cv.1)[!names(cv.1) %in% c("n.cores", "adapter")]
			for (i in fields) {
				expect_identical(
					cv.1[[i]], cv.2[[i]], info = sprintf("Field: %s", i)
				)
				if (!exceeds.core.limit(3)) {
					expect_identical(
						cv.1[[i]], cv.3[[i]], info = sprintf("Field: %s", i)
					)
				}
			}
		}
	)
}

run.tests(test.same.seeds.produce.same.results.with.custer)


#------------------------------------------------------------------------------
context("Test same seed produce same result with parameter grid with cluster")

test_that(
	"Test same seed produce same result with parameter grid.", {
		run.cv.models <- function(n.cores) {
			if (exceeds.core.limit(n.cores)) {
				return()
			}
			cv <- cv.models(
				call = gbm(
					Petal.Length ~ ., data = iris, distribution = "gaussian",
					n.cores = 1, n.trees = 5
				),
				seed = 1, n.cores = n.cores,
				grid = list(
					interaction.depth = c(1, 5), n.minobsinnode = c(1, 10)
				),
				grid.predict = list(n.trees = c(1, 3))
			)
			return(cv)
		}
		cv.1 <- run.cv.models(1)
		cv.2 <- run.cv.models(2)
		cv.20 <- run.cv.models(10)
		expect_equal(cv.1$cv.results, cv.2$cv.results, info = "1 vs 2")
		expect_equal(cv.1$cv.results, cv.20$cv.results, info = "1 vs 10")
	}
)
