require(testthat)

#-------------------------------------------------------------------------------
test_that("run cv.models with svm (regression, no cluster)", {
	data(iris)
	cv <- cv.models(
		svm, args.model = list(Sepal.Length ~ .), data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness"), n.cores = 1
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that("run cv.models with svm (classification, no cluster)", {
	data(iris)
	iris <- subset(iris, Species != "setosa")
	iris$Species <- as.numeric(iris$Species) - 2
	cv <- cv.models(
		svm, args.model = list(Species ~ .), data = iris,
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
test_that("run cv.models with svm (regression, with cluster)", {
	data(iris)
	cv <- cv.models(
		svm, args.model = list(Sepal.Length ~ .), data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness")
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that("run cv.models with svm (classification, with cluster)", {
	data(iris)
	iris <- subset(iris, Species != "setosa")
	iris$Species <- as.numeric(iris$Species) - 2
	cv <- cvctree <- cv.models(
		svm, args.model = list(Species ~ .), data = iris,
		cv.metrics = c("threshold", "auc", "mse", "rmse", "informedness"),
		seed = 2
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})


detach("package:e1071", unload=TRUE)
