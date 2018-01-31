#===============================================================================
#	Test setting same 'seed' produces same result.
#===============================================================================
library(randomForest)
library(gbm)


#------------------------------------------------------------------------------
context("Preparing tests")

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

test__same_seeds_produce_same_results <- function(call, msg) {
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

run.tests(test__same_seeds_produce_same_results)
rm(test__same_seeds_produce_same_results)


#------------------------------------------------------------------------------
context("Test different seeds produce different results")

test__different_seeds_produce_different_results <- function(call, msg) {
	test_that(
		msg, {
			cv.1 <- cv.models(call, seed = 1, n.cores = 1, n.trees = 10)
			cv.2 <- cv.models(call, seed = 2, n.cores = 1, n.trees = 10)
			fields <- names(cv.1)[!names(cv.1) %in% c("n.cores", "adapter")]
			expect_false(identical(cv.1$cv.results, cv.2$cv.results))
		}
	)
}

run.tests(test__different_seeds_produce_different_results)
rm(test__different_seeds_produce_different_results)


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

test__same_seeds_produce_same_results_with_custer <- function(call, msg) {
	test_that(
		msg, {
			cv.1 <- cv.models(call, seed = 1, n.cores = 1, n.trees = 10)
			cv.2 <- cv.models(call, seed = 1, n.cores = 2, n.trees = 10)
			cv.20 <- cv.models(call, seed = 1, n.cores = 20, n.trees = 10)
			fields <- names(cv.1)[!names(cv.1) %in% c("n.cores", "adapter")]
			for (i in fields) {
				expect_identical(
					cv.1[[i]], cv.2[[i]], info = sprintf("Field: %s", i)
				)
				expect_identical(
					cv.1[[i]], cv.20[[i]], info = sprintf("Field: %s", i)
				)
			}
		}
	)
}

run.tests(test__same_seeds_produce_same_results_with_custer)
rm(test__same_seeds_produce_same_results_with_custer)


#------------------------------------------------------------------------------
context("Test same seed produce same result with parameter grid with cluster")

test_that(
	"Test same seed produce same result with parameter grid.", {
		run.cv.models <- function(n.cores) {
			cv <- cv.models(
				call = gbm(
					Petal.Length ~ ., data = iris, distribution = "gaussian",
					n.cores = 1, n.trees = 5
				),
				seed = 1, n.cores = n.cores,
				grid = list(
					interaction.depth = c(1, 5), n.minobsinnode = c(1, 10)
				),
				grid.predict = list(n.trees = 1:3)
			)
			return(cv)
		}
		cv.1 <- run.cv.models(1)
		cv.2 <- run.cv.models(2)
		cv.20 <- run.cv.models(20)
		expect_equal(cv.1$cv.results, cv.2$cv.results, info = "1 vs 2")
		expect_equal(cv.1$cv.results, cv.20$cv.results, info = "1 vs 20")
	}
)

rm(run.tests)
