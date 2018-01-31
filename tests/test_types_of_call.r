#===============================================================================
#	Test different types of call produce same results.
#
#	This test check whetehr direct input, via call(), via substitute(), call in
#	model object can produce same results.
#===============================================================================
library(randomForest)
library(gbm)

#------------------------------------------------------------------------------
context("Test different types of call produce same result")

test_that(
	"Test different types of call produce same result with glm().", {
		# Import functions.
		source("utils.r", encoding = "UTF-8")
		# Prepare calls and object.
		data(iris)
		call.call <- call("glm", Petal.Length ~ ., data = iris)
		call.substitute <- substitute(glm(Petal.Length ~ ., data = iris))
		object <- glm(Petal.Length ~ ., data = iris)
		# Run cross validation.
		cv.default <- cv.models(
			glm(Petal.Length ~ ., data = iris), seed = 1, n.cores = 1
		)
		cv.call <- cv.models(call.call, seed = 1, n.cores = 1)
		cv.substitute <- cv.models(call.substitute, seed = 1, n.cores = 1)
		cv.object <- cv.models(object, seed = 1, n.cores = 1)
		# Test consistency of the results.
		expect_true(has.same.results(cv.default, cv.call))
		expect_true(has.same.results(cv.default, cv.substitute))
		expect_true(has.same.results(cv.default, cv.object))
	}
)

test_that(
	"Test different types of call produce same result with gbm().", {
		# Import functions.
		source("utils.r", encoding = "UTF-8")
		# Prepare calls and object.
		data(iris)
		call.call <- call(
			"gbm", Petal.Length ~ ., data = iris, distribution = "gaussian"
		)
		call.substitute <- substitute(
			gbm(Petal.Length ~ ., data = iris, distribution = "gaussian")
		)
		object <- gbm(Petal.Length ~ ., data = iris, distribution = "gaussian")
		# Run cross validation.
		cv.default <- cv.models(
			gbm(Petal.Length ~ ., data = iris, distribution = "gaussian"),
			n.trees = 100, seed = 1, n.cores = 1
		)
		cv.call <- cv.models(call.call, n.trees = 100, seed = 1, n.cores = 1)
		cv.substitute <- cv.models(
			call.substitute, n.trees = 100, seed = 1, n.cores = 1
		)
		cv.object <- cv.models(object, n.trees = 100, seed = 1, n.cores = 1)
		# Test consistency of the results.
		expect_true(has.same.results(cv.default, cv.call))
		expect_true(has.same.results(cv.default, cv.substitute))
		expect_true(has.same.results(cv.default, cv.object))
	}
)
