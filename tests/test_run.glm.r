#===============================================================================
#	Test suite for running GLM.
#===============================================================================
library(testthat)
library(cv.models)


#-------------------------------------------------------------------------------
#	Tests for regression by GLM.
#-------------------------------------------------------------------------------

# GLM regression.
run.glm.regression <- function() {
	cv <- cv.models(
		glm(Petal.Length ~ ., data = iris, weights = Sepal.Width), n.cores = 2
	)
	print(cv)
}

# GLM regression without cluster.
run.glm.regression.no.cluster <- function() {
	cv <- cv.models(
		glm(Petal.Length ~ ., data = iris, weights = Sepal.Width), n.cores = 1
	)
	print(cv)
}

# GLM regression with weights.
run.glm.regression.with.weights <- function() {
	cv <- cv.models(
		glm(Petal.Length ~ ., data = iris, weights = iris$Sepal.Width),
		n.cores = 2
	)
	print(cv)
}

# GLM regression with weights without cluster.
run.glm.regression.with.weights.no.cluster <- function() {
	cv <- cv.models(
		glm(Petal.Length ~ ., data = iris, weights = iris$Sepal.Width),
		n.cores = 1
	)
	print(cv)
}

# GLM regression with "join".
run.glm.regression.join <- function() {
	cv <- cv.models(
		glm(Petal.Length ~ ., data = iris, weights = Sepal.Width), n.cores = 2,
		aggregate.method = "join"
	)
	print(cv)
}

# GLM regression with "join" without cluster.
run.glm.regression.join.no.cluster <- function() {
	cv <- cv.models(
		glm(Petal.Length ~ ., data = iris, weights = Sepal.Width), n.cores = 1,
		aggregate.method = "join"
	)
	print(cv)
}

# GLM with external formula.
run.glm.regression.with.external.formula <- function() {
	f <- Petal.Length ~ .
	cv <- cv.models(
		glm(f, data = iris, weights = Sepal.Width), n.cores = 2
	)
	print(cv)
}

# GLM with external formula.
run.glm.regression.with.external.formula.no.cluster <- function() {
	f <- Petal.Length ~ .
	cv <- cv.models(
		glm(f, data = iris, weights = Sepal.Width), n.cores = 1
	)
	print(cv)
}


#-------------------------------------------------------------------------------
#	Tests for classification by GLM.
#-------------------------------------------------------------------------------

# Create dataset. Obtained from help page of glmmML and slightly modified.
create.dataset <- function() {
	id <- factor(rep(1:20, rep(5, 20)))
	y <- rbinom(100, prob = rep(runif(20), rep(5, 20)), size = 1)
	x <- rnorm(100)
	w <- rgamma(100, shape = 0.1)
	dat <- data.frame(y = y, x = x, w = w, id = id)
	return(dat)
}

# GLM classification.
run.glm.classification <- function() {
	dat <- create.dataset()
	cv <- cv.models(glm(y ~ x, data = dat, family = binomial), n.cores = 2)
	print(cv)
}

# GLM classification without cluster.
run.glm.classification.no.cluster <- function() {
	dat <- create.dataset()
	cv <- cv.models(glm(y ~ x, data = dat, family = binomial), n.cores = 1)
	print(cv)
}

# GLM classification with weights.
run.glm.classification.with.weights <- function() {
	dat <- create.dataset()
	cv <- cv.models(
		glm(y ~ x, data = dat, weights = w, family = binomial), n.cores = 2
	)
	print(cv)
}

# GLM classification with weights without cluster.
run.glm.classification.with.weights.no.cluster <- function() {
	dat <- create.dataset()
	cv <- cv.models(
		glm(y ~ x, data = dat, weights = w, family = binomial), n.cores = 1
	)
	print(cv)
}

# GLM classification with join.
run.glm.classification.join <- function() {
	dat <- create.dataset()
	cv <- cv.models(
		glm(y ~ x, data = dat, weights = w, family = binomial), n.cores = 2,
		aggregate.method = "join"
	)
	print(cv)
}

# GLM classification with join without cluster.
run.glm.classification.join.no.cluster <- function() {
	dat <- create.dataset()
	cv <- cv.models(
		glm(y ~ x, data = dat, weights = w, family = binomial), n.cores = 1,
		aggregate.method = "join"
	)
	print(cv)
}

# GLM classification with external formula.
run.glm.classification.with.external.formula <- function() {
	dat <- create.dataset()
	f <- y ~ x
	cv <- cv.models(
		glm(f, data = dat, weights = w, family = binomial), n.cores = 2
	)
	print(cv)
}

# GLM classification with external formula without cluster.
run.glm.classification.with.external.formula.no.cluster <- function() {
	dat <- create.dataset()
	f <- y ~ x
	cv <- cv.models(
		glm(f, data = dat, weights = w, family = binomial), n.cores = 1
	)
	print(cv)
}


#-------------------------------------------------------------------------------
#	Run all tests for GLM.
#-------------------------------------------------------------------------------

do.test.that <- function(fun) {
	msg <- gsub("\\.", " ", deparse(substitute(fun)))
	test_that(msg, fun())
}

do.test.that(run.glm.regression)
do.test.that(run.glm.regression.no.cluster)
do.test.that(run.glm.regression.with.weights)
do.test.that(run.glm.regression.with.weights.no.cluster)
do.test.that(run.glm.regression.join)
do.test.that(run.glm.regression.join.no.cluster)
do.test.that(run.glm.regression.with.external.formula)
do.test.that(run.glm.regression.with.external.formula.no.cluster)

do.test.that(run.glm.classification)
do.test.that(run.glm.classification.no.cluster)
do.test.that(run.glm.classification.with.weights)
do.test.that(run.glm.classification.with.weights.no.cluster)
do.test.that(run.glm.classification.join)
do.test.that(run.glm.classification.join.no.cluster)
do.test.that(run.glm.classification.with.external.formula)
do.test.that(run.glm.classification.with.external.formula.no.cluster)
