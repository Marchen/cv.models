require(testthat)

#-------------------------------------------------------------------------------
test_that("run cv.models with lm (regression, no cluster)", {
	data(iris)
	cv <- cv.models(
		lm, args.model = list(Sepal.Length ~ .), data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness"), n.cores = 1
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that("run cv.models with lm (regression, with cluster)", {
	data(iris)
	cv <- cv.models(
		lm, args.model = list(Sepal.Length ~ .), data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness")
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

