#===============================================================================
#	Test suite for running GBM.
#===============================================================================
library(testthat)
library(cv.models)
library(gbm)


#-------------------------------------------------------------------------------
#	Tests for regression by GBM.
#-------------------------------------------------------------------------------

# GBM regression.
run.gbm.regression <- function() {
	cv <- cv.models(
		gbm(
			Petal.Length ~ ., data = iris, weights = iris$Sepal.Width,
			distribution = "gaussian", n.trees = 10, n.cores = 1
		),
		n.trees = 10
	)
}

# GBM regression without cluster.
run.gbm.regression.no.cluster <- function() {
	cv <- cv.models(
		gbm(
			Petal.Length ~ ., data = iris, weights = iris$Sepal.Width,
			distribution = "gaussian", n.trees = 10, n.cores = 1
		),
		n.trees = 10, n.cores = 1
	)
}

# GBM regression with grid search.
run.gbm.regression.with.grid <- function() {
	cv <- cv.models(
		gbm(
			Petal.Length ~ ., data = iris, weights = iris$Sepal.Width,
			distribution = "gaussian", n.trees = 10, n.cores = 1
		),
		grid = list(interaction.depth = c(1, 5), n.minobsinnode = c(1, 10)),
		n.trees = 10
	)
}

# GBM regression with grid search without cluster.
run.gbm.regression.with.grid.no.cluster <- function() {
	cv <- cv.models(
		gbm(
			Petal.Length ~ ., data = iris, weights = iris$Sepal.Width,
			distribution = "gaussian", n.trees = 10, n.cores = 1
		),
		grid = list(interaction.depth = c(1, 5), n.minobsinnode = c(1, 10)),
		n.trees = 10, n.cores = 1
	)
}

# GBM regression with grid search for predct args.
run.gbm.regression.with.grid.predict <- function() {
	cv <- cv.models(
		gbm(
			Petal.Length ~ ., data = iris, weights = iris$Sepal.Width,
			distribution = "gaussian", n.trees = 100, n.cores = 1
		),
		grid.predict = list(n.trees = c(5, 10, 50, 80))
	)
}

# GBM regression with grid search for predct args without cluster.
run.gbm.regression.with.grid.predict.no.cluster <- function() {
	cv <- cv.models(
		gbm(
			Petal.Length ~ ., data = iris, weights = iris$Sepal.Width,
			distribution = "gaussian", n.trees = 100, n.cores = 1
		),
		grid.predict = list(n.trees = c(5, 10, 50, 80)), n.cores = 1
	)
}

# GBM regression with "join".
run.gbm.regression.join <- function() {
	cv <- cv.models(
		gbm(
			Petal.Length ~ ., data = iris, weights = iris$Sepal.Width,
			distribution = "gaussian", n.trees = 10, n.cores = 1
		),
		n.trees = 10, aggregate.method = "join"
	)
}

# GBM regression with "join" without cluster.
run.gbm.regression.join.no.cluster <- function() {
	cv <- cv.models(
		gbm(
			Petal.Length ~ ., data = iris, weights = iris$Sepal.Width,
			distribution = "gaussian", n.trees = 10, n.cores = 1
		),
		n.trees = 10, aggregate.method = "join", n.cores = 1
	)
}

run.gbm.regression.external.formula <- function() {
	f <- Petal.Length ~ .
	cv <- cv.models(
		gbm(
			f, data = iris, weights = Sepal.Width,
			distribution = "gaussian", n.trees = 100
		),
		grid.predict = list(n.trees = c(5, 10, 50, 80)), n.cores = 1
	)

}


#-------------------------------------------------------------------------------
#	Tests for classification by GBM.
#-------------------------------------------------------------------------------

# GBM classification.
run.gbm.classification <- function() {
	cv <- cv.models(
		gbm(
			Species ~ ., data = iris, weights = iris$Sepal.Width,
			distribution = "multinomial", n.trees = 10, n.cores = 1
		),
		n.trees = 10, positive.class = "virginica"
	)
}

# GBM classification without cluster.
run.gbm.classification.no.cluster <- function() {
	cv <- cv.models(
		gbm(
			Species ~ ., data = iris, weights = iris$Sepal.Width,
			distribution = "multinomial", n.trees = 10, n.cores = 1
		),
		n.trees = 10, positive.class = "virginica", n.cores = 1
	)
}

# GBM classification with grid.
run.gbm.classification.with.grid <- function() {
	cv <- cv.models(
		gbm(
			Species ~ ., data = iris, weights = iris$Sepal.Width,
			distribution = "multinomial", n.trees = 10, n.cores = 1
		),
		grid = list(interaction.depth = c(1, 5), n.minobsinnode = c(1, 10)),
		n.trees = 10, positive.class = "virginica"
	)
}

# GBM classification with grid without cluster.
run.gbm.classification.with.grid.no.cluster <- function() {
	cv <- cv.models(
		gbm(
			Species ~ ., data = iris, weights = iris$Sepal.Width,
			distribution = "multinomial", n.trees = 10, n.cores = 1
		),
		grid = list(interaction.depth = c(1, 5), n.minobsinnode = c(1, 10)),
		n.trees = 10, positive.class = "virginica", n.cores = 1
	)
}

# GBM classification with grid for prediction.
run.gbm.classification.with.grid.predict <- function() {
	cv <- cv.models(
		gbm(
			Species ~ ., data = iris, weights = iris$Sepal.Width,
			distribution = "multinomial", n.trees = 100, n.cores = 1
		),
		grid.predict = list(n.trees = c(5, 10, 50, 80)),
		n.trees = 10, positive.class = "virginica"
	)

}

# GBM classification with grid for prediction without cluster.
run.gbm.classification.with.grid.predict.no.cluster <- function() {
	cv <- cv.models(
		gbm(
			Species ~ ., data = iris, weights = iris$Sepal.Width,
			distribution = "multinomial", n.trees = 100, n.cores = 1
		),
		grid.predict = list(n.trees = c(5, 10, 50, 80)),
		n.trees = 10, positive.class = "virginica", n.cores = 1
	)
}

# GBM classification.
run.gbm.classification.join <- function() {
	cv <- cv.models(
		gbm(
			Species ~ ., data = iris, weights = iris$Sepal.Width,
			distribution = "multinomial", n.trees = 10, n.cores = 1
		),
		n.trees = 10, positive.class = "virginica", aggregate.method = "join"
	)
}

# GBM classification without cluster.
run.gbm.classification.join.no.cluster <- function() {
	cv <- cv.models(
		gbm(
			Species ~ ., data = iris, weights = iris$Sepal.Width,
			distribution = "multinomial", n.trees = 10, n.cores = 1
		),
		n.trees = 10, positive.class = "virginica", aggregate.method = "join",
		n.cores = 1
	)
}


#-------------------------------------------------------------------------------
#	Run all tests for GBM.
#-------------------------------------------------------------------------------
context("Test compatibility with gbm")

do.test.that <- function(fun) {
	msg <- deparse(substitute(fun))
	test_that(msg, expect_silent(fun()))
}

do.test.that(run.gbm.regression)
do.test.that(run.gbm.regression.no.cluster)
do.test.that(run.gbm.regression.with.grid)
do.test.that(run.gbm.regression.with.grid.no.cluster)
do.test.that(run.gbm.regression.with.grid.predict)
do.test.that(run.gbm.regression.with.grid.predict.no.cluster)
do.test.that(run.gbm.regression.join)
do.test.that(run.gbm.regression.join.no.cluster)
do.test.that(run.gbm.regression.external.formula)

do.test.that(run.gbm.classification)
do.test.that(run.gbm.classification.no.cluster)
do.test.that(run.gbm.classification.with.grid)
do.test.that(run.gbm.classification.with.grid.no.cluster)
do.test.that(run.gbm.classification.with.grid.predict)
do.test.that(run.gbm.classification.with.grid.predict.no.cluster)
do.test.that(run.gbm.classification.join)
do.test.that(run.gbm.classification.join.no.cluster)

