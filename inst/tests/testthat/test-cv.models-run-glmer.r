require(testthat)

#-------------------------------------------------------------------------------
test_that("run cv.models with glmer (no cluster)", {
	data(iris)
	iris <- subset(iris, Species != "setosa")
	iris$Species <- as.numeric(iris$Species) - 2
	iris$Random <- c(rep(1:5, 10), rep(6:10, 10))
	cv <- cv.models(
		glmer, args.model = list(Species ~ Sepal.Length + (1|Random), family = binomial),
 		data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness"), n.cores = 1
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that("run cv.models with glmer (with cluster)", {
	data(iris)
	iris <- subset(iris, Species != "setosa")
	iris$Species <- as.numeric(iris$Species) - 2
	iris$Random <- c(rep(1:5, 10), rep(6:10, 10))
	cv <- cv.models(
		glmer, args.model = list(Species ~ Sepal.Length + (1|Random), family = binomial),
 		data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness")
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})









