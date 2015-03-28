require(testthat)

#-------------------------------------------------------------------------------
test_that("run cv.models with lme (no cluster)", {
	data(iris)
	cv <- cv.models(
		lme, args.model = list(Sepal.Length ~ . - Species, random = ~ 1 | Species),
 		data = iris,
		cv.metrics = c("r.squared", "mse", "rmse"), n.cores = 1
	)
	print(cv)
	bm <- get.best.models(cv, metrics = c("r.squared", "mse", "rmse"))
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that("run cv.models with lme (with cluster)", {
	data(iris)
	cv <- cv.models(
		lme, args.model = list(Sepal.Length ~ . - Species, random = ~ 1 | Species),
 		data = iris,
		cv.metrics = c("r.squared", "mse", "rmse")
	)
	print(cv)
	bm <- get.best.models(cv, metrics = c("r.squared", "mse", "rmse"))
	print(bm)
	summary(bm)
})


detach("package:nlme", unload=TRUE)

