require(testthat)

#-------------------------------------------------------------------------------
test_that("run cv.models with ctree (regression, no cluster)", {
	data(iris)
	cv <- cv.models(
		ctree, args.model = list(Sepal.Length ~ .), data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness"), n.cores = 1
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that("run cv.models with ctree (classification, no cluster)", {
	data(iris)
	iris <- subset(iris, Species != "setosa")
	iris$Species <- as.numeric(iris$Species) - 2
	cv <- cv.models(
		ctree, args.model = list(Species ~ .), data = iris,
		cv.metrics = c("threshold", "auc", "mse", "rmse", "informedness"),
		n.cores = 1,
		seed = 2
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that("run cv.models with ctree (regression, with cluster)", {
	data(iris)
	cv <- cv.models(
		ctree, args.model = list(Sepal.Length ~ .), data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness")
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that("run cv.models with ctree (classification, with cluster)", {
	data(iris)
	iris <- subset(iris, Species != "setosa")
	iris$Species <- as.numeric(iris$Species) - 2
	cv <- cv.models(
		ctree, args.model = list(Species ~ .), data = iris,
		cv.metrics = c("threshold", "auc", "mse", "rmse", "informedness"),
		seed = 2
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

detach("package:party", unload = TRUE)
