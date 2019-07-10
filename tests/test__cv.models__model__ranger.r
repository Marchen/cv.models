#=============================================================================
#	Test suite for running ranger.
#=============================================================================
library(testthat)
library(cv.models)
library(ranger)

set.seed(123)

#-----------------------------------------------------------------------------
#	Tests for regression by ranger.
#-----------------------------------------------------------------------------

# ranger regression.
run.ranger.regression <- function() {
	cv <- cv.models(ranger(Petal.Length ~ ., data = iris, num.trees = 10))
}

# ranger regression without cluster.
run.ranger.regression.no.cluster <- function() {
	cv <- cv.models(
		ranger(Petal.Length ~ ., data = iris, num.trees = 10), n.cores = 1
	)
}

# ranger regression with "join".
run.ranger.regression.join <- function() {
	cv <- cv.models(
		ranger(Petal.Length ~ ., data = iris, num.trees = 10),
		aggregate.method = "join"
	)
}

# ranger classification with grid.
run.ranger.regression.with.grid <- function() {
	cv <- cv.models(
		ranger(Petal.Length ~ ., data = iris, num.trees = 10),
		grid = list(max.depth = c(1, 5), min.node.size = c(1, 10))
	)
}

# ranger classification with grid without cluster.
run.ranger.regression.with.grid.no.cluster <- function() {
	cv <- cv.models(
		ranger(Petal.Length ~ ., data = iris, num.trees = 10), n.cores = 1,
		grid = list(max.depth = c(1, 5), min.node.size = c(1, 10))
	)
}

# ranger regression with "join" without cluster.
run.ranger.regression.join.no.cluster <- function() {
	cv <- cv.models(
		ranger(Petal.Length ~ ., data = iris, num.trees = 10),
		aggregate.method = "join", n.cores = 1
	)
}

# ranger regressin with external formula.
run.ranger.regression.external.formula <- function() {
	f <- Petal.Length ~ .
	cv <- cv.models(ranger(f, data = iris, num.trees = 10))
}

# ranger regressin with external formula without cluster.
run.ranger.regression.external.formula.no.cluster <- function() {
	f <- Petal.Length ~ .
	cv <- cv.models(ranger(f, data = iris, num.trees = 10), n.cores = 1)
}


#-----------------------------------------------------------------------------
#	Tests for classification by ranger.
#-----------------------------------------------------------------------------

# ranger classification.
run.ranger.classification <- function() {
	cv <- cv.models(
		ranger(
			Species ~ ., data = iris, case.weights = iris$Sepal.Width,
			num.trees = 10
		),
		positive.class = "virginica", n.cores = 2
	)
}

# ranger classification without cluster.
run.ranger.classification.no.cluster <- function() {
	cv <- cv.models(
		ranger(
			Species ~ ., data = iris, case.weights = iris$Sepal.Width,
			num.trees = 10
		),
		positive.class = "virginica", n.cores = 1
	)
}

# ranger classification with grid.
run.ranger.classification.with.grid <- function() {
	cv <- cv.models(
		ranger(
			Species ~ ., data = iris, case.weights = iris$Sepal.Width,
			num.trees = 10
		),
		grid = list(max.depth = c(2, 5), min.node.size = c(2, 10)),
		positive.class = "virginica"
	)
}

# ranger classification with grid without cluster.
run.ranger.classification.with.grid.no.cluster <- function() {
	cv <- cv.models(
		ranger(
			Species ~ ., data = iris, case.weights = iris$Sepal.Width,
			num.trees = 10
		),
		grid = list(max.depth = c(1, 5), min.node.size = c(1, 10)),
		positive.class = "virginica", n.cores = 1
	)
}

# ranger classification.
run.ranger.classification.join <- function() {
	cv <- cv.models(
		ranger(
			Species ~ ., data = iris, case.weights = iris$Sepal.Width,
			num.trees = 10
		),
		positive.class = "virginica", aggregate.method = "join"
	)
}

# ranger classification without cluster.
run.ranger.classification.join.no.cluster <- function() {
	cv <- cv.models(
		ranger(
			Species ~ ., data = iris, case.weights = iris$Sepal.Width,
			num.trees = 10
		),
		positive.class = "virginica", aggregate.method = "join", n.cores = 1
	)
}

# ranger classification with external formula.
run.ranger.classification.external.formula <- function() {
	f <- Species ~ .
	cv <- cv.models(
		ranger(
			f, data = iris, case.weights = iris$Sepal.Width, num.trees = 10
		),
		positive.class = "virginica", n.cores = 2
	)
}

# ranger classification without cluster.
run.ranger.classification.external.formula.no.cluster <- function() {
	f <- Species ~ .
	cv <- cv.models(
		ranger(
			f, data = iris, case.weights = iris$Sepal.Width, num.trees = 10
		),
		positive.class = "virginica", n.cores = 1
	)
}


#-----------------------------------------------------------------------------
#	Run tests.
#-----------------------------------------------------------------------------
do.test.that <- function(fun) {
	msg <- deparse(substitute(fun))
	test_that(msg, fun())
}

do.test.that(run.ranger.regression)
do.test.that(run.ranger.regression.no.cluster)
do.test.that(run.ranger.regression.with.grid)
do.test.that(run.ranger.regression.with.grid.no.cluster)
do.test.that(run.ranger.regression.join)
do.test.that(run.ranger.regression.join.no.cluster)
do.test.that(run.ranger.regression.external.formula)
do.test.that(run.ranger.regression.external.formula.no.cluster)

do.test.that(run.ranger.classification)
do.test.that(run.ranger.classification.no.cluster)
do.test.that(run.ranger.classification.with.grid)
do.test.that(run.ranger.classification.with.grid.no.cluster)
do.test.that(run.ranger.classification.join)
do.test.that(run.ranger.classification.join.no.cluster)
do.test.that(run.ranger.classification.external.formula)
do.test.that(run.ranger.classification.external.formula.no.cluster)
