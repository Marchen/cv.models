require(testthat)

#-------------------------------------------------------------------------------
test_that("run cv.models with lmer (no cluster)", {
	data(iris)
	cv <- cv.models(
		lmer, args.model = list(Sepal.Length ~ . + (1|Species)),
 		data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness"), n.cores = 1
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})


#-------------------------------------------------------------------------------
test_that("run cv.models with lmer (with cluster)", {
	data(iris)
	cv <- cv.models(
		lmer, args.model = list(Sepal.Length ~ . + (1|Species)),
 		data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness")
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})




