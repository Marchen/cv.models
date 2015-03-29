require(testthat)

#-------------------------------------------------------------------------------
test_that("run cv.models with glmmML (no cluster)", {
	data(iris)
	iris <- subset(iris, Species != "setosa")
	iris$Species <- as.numeric(iris$Species) - 2
	iris$Random <- factor(c(rep(1:5, 10), rep(6:10, 10)))
	cv <- cv.models(
		glmmML, args.model = args.model(
			Species ~ Sepal.Length, cluster = Random, family = binomial
		),
 		data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness"), n.cores = 1
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that("run cv.models with glmmML (with cluster)", {
	data(iris)
	iris <- subset(iris, Species != "setosa")
	iris$Species <- as.numeric(iris$Species) - 2
	iris$Random <- c(rep(1:5, 10), rep(6:10, 10))
	cv <- cv.models(
		glmmML, args.model = args.model(
			Species ~ Sepal.Length, cluster = Random, family = binomial
		),
 		data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness")
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})


detach("package:glmmML", unload = TRUE)


