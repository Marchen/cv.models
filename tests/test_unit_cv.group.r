#==============================================================================
#	Unit test of cv.group with class stratification
#==============================================================================
library(testthat)
library(cv.models)
library(randomForest)


#------------------------------------------------------------------------------
context("Unit test for cv.group with class stratification.")


#------------------------------------------------------------------------------
#	Create biased iris data.
#------------------------------------------------------------------------------
biased.iris <- function() {
	iris2 <- iris[Species != "setosa", ]
	iris2 <- rbind(iris2, head(iris[iris$Species == "setosa", ], 10))
	return(iris2)
}


#------------------------------------------------------------------------------
#	Create cv.models object for testing.
#------------------------------------------------------------------------------
create.test.cv.models.object <- function(stratify) {
	iris2 <- biased.iris()
	call <- substitute(randomForest(Species ~ ., data = iris2))
	object <- list(
		call = call, folds = 10, stratify = stratify,
		adapter = model.adapter$new(call, environment(), NULL)
	)
	class(object) <- "cv.models"
	return(object)
}


#------------------------------------------------------------------------------
#	Create test data.
#------------------------------------------------------------------------------
create.test.data.for.stratification <- function(stratify) {
	object <- create.test.cv.models.object(stratify)
	group <- cv.models:::cv.group(object)
	y <- object$adapter$y.vars[[1]]
	test.df <- data.frame(y = y, group = group)
	test.df.split <- split(test.df, group)
	return(test.df.split)
}


#------------------------------------------------------------------------------
#	Try grouping and test all groups have "setosa".
#------------------------------------------------------------------------------
try.and.test.having.setosa <- function(stratify, times = 10) {
	results <- rep(NA, times)
	for (i in 1:times) {
		test.df.split <- create.test.data.for.stratification(
			stratify = stratify
		)
		has.setosa <- sapply(test.df.split, function(x) "setosa" %in% x$y)
		results[i] <- all(has.setosa)
	}
	return(results)
}


#------------------------------------------------------------------------------
test_that(
	"Test cv.group assigns same number of observations for each group.", {
		try.test <- function(stratify) {
			object <- create.test.cv.models.object(stratify = stratify)
			group <- cv.models:::cv.group(object)
			n.obs <- tapply(group, group, length)
			expect_true(
				length(unique(n.obs)) <= 2,
				info = sprintf(
					"Test number of group sizes <= 2: stratify = %s",
					stratify
				)
			)
			expect_true(
				(range(n.obs)[2] - range(n.obs)[1]) %in% 0:1,
				info = sprintf(
					"Test all groups have (nearly) equal size: stratify = %s",
					stratify
				)
			)
		}
		# To avoid occasional pass of test, try 10 times.
		for (i in 1:10) {
			try.test(stratify = FALSE)
			try.test(stratify = TRUE)
		}
	}
)


#------------------------------------------------------------------------------
test_that(
	"Test 'stratify = TRUE' makes stratified groups.", {
		# Check all groups have "setosa".
		# Because splitting use stochastic process, try 10 times to avoid
		# passing the test by occasional stratification.
		results <- try.and.test.having.setosa(stratify = TRUE, times = 10)
		expect_true(all(results), info = "Test all groups have setosa.")
	}
)


#------------------------------------------------------------------------------
test_that(
	"Test 'stratify = FALSE' makes non-stratified groups.", {
		# Check some groups don't have "setosa".
		# Because splitting use stochastic process, try 10 times to avoid
		# failing the test by occasional stratification.
		results <- try.and.test.having.setosa(stratify = FALSE, times = 10)
		expect_false(
			all(results), info = "Test some groups don't have setosa."
		)
	}
)


#------------------------------------------------------------------------------
test_that(
	"Test 'stratify = TRUE' for regression model produce error.", {
		call <- substitute(randomForest(Petal.Length ~ ., data = iris))
		object <- list(
			call = call, folds = 10, stratify = TRUE,
			adapter = model.adapter$new(call, environment(), NULL)
		)
		class(object) <- "cv.models"
		# regexp <- paste0(
		# 	"Currently, class stratification is not supported for \n",
		# 	"regression models\\."
		# )
		# expect_error(cv.models:::cv.group(object), regexp)
		expect_error(cv.models:::cv.group(object))
	}
)


#------------------------------------------------------------------------------
test_that(
	"Test produce warning when 'folds' > minimum number of observations", {
		iris2 <- biased.iris()
		call <- substitute(randomForest(Species ~ ., data = iris2))
		object <- list(
			call = call, folds = 20, stratify = TRUE,
			adapter = model.adapter$new(call, environment(), NULL)
		)
		class(object) <- "cv.models"
		# regexp <- paste0(
		# 	"Number\\(s\\) of observations in a class of response variable \n",
		# 	"is smaller than the number of 'fold'\\."
		# )
		# expect_warning(cv.models:::cv.group(object), regexp)
		expect_warning(cv.models:::cv.group(object))
	}
)

rm(
	biased.iris,
	create.test.cv.models.object,
	create.test.data.for.stratification,
	try.and.test.having.setosa
)


#==============================================================================
#	Unit test of cv.group with user defined group
#==============================================================================

context("Unit test for cv.group with user defined group.")

test_that(
	"Test user defined group with different variable types", {
		create.test.object <- function(group) {
			call <- substitute(randomForest(Petal.Length ~ ., data = iris))
			object <- list(
				call = call, group = group,
				adapter = model.adapter$new(call, environment(), NULL)
			)
			class(object) <- "cv.models"
			return(object)
		}
		# Test number of groups.
		obj <- create.test.object(rep(c("a","b"), each = 75))
		expect_equal(length(unique(cv.models:::cv.group(obj))), 2)
		obj <- create.test.object(rep(1:3, each = 50))
		expect_equal(length(unique(cv.models:::cv.group(obj))), 3)
		obj <- create.test.object(rep(c(TRUE, FALSE), each = 75))
		expect_equal(length(unique(cv.models:::cv.group(obj))), 2)
		obj <- create.test.object(iris$Species)
		expect_equal(length(unique(cv.models:::cv.group(obj))), 3)
		# Test invalid group actually produce error.
		obj <- create.test.object(list(1))
		expect_error(cv.models:::cv.group(obj))
	}
)
