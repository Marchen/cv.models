require(testthat)

#-------------------------------------------------------------------------------
test_that("run cv.models with glm (regression, no cluster)", {
	data(iris)
	cv <- cv.models(
		glm, args.model = list(Sepal.Length ~ ., family = gaussian), data = iris,
		cv.metrics = c("mse", "rmse", "r.squared"), n.cores = 1
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that("run cv.models with glm (classification, no cluster)", {
	data(iris)
	iris <- subset(iris, Species != "setosa")
	iris$Species <- as.numeric(iris$Species) - 2
	cv <- cv.models(
		glm, args.model = list(Species ~ Petal.Length, family = binomial),
		data = iris,
		cv.metrics = c("auc", "informedness", "ppv", "npv"), n.cores = 1
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that("run cv.models with glm (regression, with cluster)", {
	data(iris)
	cv <- cv.models(
		glm, args.model = list(Sepal.Length ~ ., family = gaussian), data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness")
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that("run cv.models with glm (classification, with cluster)", {
	data(iris)
	iris <- subset(iris, Species != "setosa")
	iris$Species <- as.numeric(iris$Species) - 2
	cv <- cv.models(
		glm, args.model = list(Species ~ Petal.Length, family = binomial),
		data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness")
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})





