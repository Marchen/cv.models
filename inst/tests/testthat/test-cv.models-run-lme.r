require(testthat)

#-------------------------------------------------------------------------------
test_that("run cv.models with lme (no cluster)", {
	data(iris)
	cv <- cv.models(
		lme, args.model = list(Sepal.Length ~ . - Species, random = ~ 1 | Species),
 		data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness"), n.cores = 1
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that("run cv.models with lme (with cluster)", {
	data(iris)
	cv <- cv.models(
		lme, args.model = list(Sepal.Length ~ . - Species, random = ~ 1 | Species),
 		data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness")
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})


detach("package:nlme", unload=TRUE)

