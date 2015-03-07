require(testthat)

#-------------------------------------------------------------------------------
test_that("run cv.models with mgcv::gam (regression, no cluster)", {
	data(iris)
	cv <- cv.models(
		gam, args.model = list(Sepal.Length ~ ., family = gaussian), data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness"), n.cores = 1,
		package.name = "gam"
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that("run cv.models with mgcv::gam (classification, no cluster)", {
	data(iris)
	iris <- subset(iris, Species != "setosa")
	iris$Species <- as.numeric(iris$Species) - 2
	cv <- cv.models(
		gam, args.model = list(Species ~ Petal.Length, family = binomial),
		data = iris,
		cv.metrics = c("threshold", "auc", "mse", "rmse", "informedness"),
		n.cores = 1, package.name = "gam"
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that("run cv.models with mgcv::gam (regression, with cluster)", {
	data(iris)
	cv <- cv.models(
		gam, args.model = list(Sepal.Length ~ ., family = gaussian), data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness"),
		package.name = "gam"
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that("run cv.models with mgcv::gam (classification, with cluster)", {
	data(iris)
	iris <- subset(iris, Species != "setosa")
	iris$Species <- as.numeric(iris$Species) - 2
	cv <- cv.models(
		gam, args.model = list(Species ~ Petal.Length, family = binomial),
		data = iris,
		cv.metrics = c("threshold", "auc", "mse", "rmse", "informedness"),
		package.name = "gam"
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

detach("package:gam", unload=TRUE)

