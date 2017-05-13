require(testthat)


#-------------------------------------------------------------------------------
test_that("Test confusion matrix is identical ", {
	data(iris)
	iris <- subset(iris, Species != "setosa")
	iris$Species <- as.numeric(iris$Species) - 2
	cv <- cv.models(
		glm, args.model = list(Species ~ Petal.Length, family = binomial),
		data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness", "threshold"),
		n.cores = 1
	)
	bm <- get.best.models(cv)
	for (i in 1:length(bm)){
		cm <- confusion.matrix(
			bm[[i]]$cv.response, bm[[i]]$cv.prediction,
			bm[[i]]$cv.metrics[1, "threshold"]
		)
		expect_identical(cm[[1]], bm[[i]]$confusion.matrix)
		print(identical(cm[[1]], bm[[i]]$confusion.matrix))
	}
	plot(bm[[1]]$cv.response, iris$Species)
})


test_that(
	"run cv.models with gbm (classification, with parameter tuning, no cluster)",
{
	data(iris)
	iris <- subset(iris, Species != "setosa")
	iris$Species <- as.numeric(iris$Species) - 2
	cv <- cv.models(
		gbm, args.model = list(
			Species~., shrinkage = c(0.1, 0.01, 0.001),
			n.minobsinnode = c(1, 5, 10), interaction.depth = c(1, 3),
			distribution = "bernoulli"
		),
		args.predict = list(n.trees = seq(5, 500, by = 5)),
		data=iris,
		cv.metrics = c("threshold", "auc", "mse", "rmse", "informedness"),
		n.cores = 1
	)
#	print(cv)
	bm <- get.best.models(cv)
	plot(bm[[1]]$cv.response, iris$Species)
	print(bm)
	summary(bm)
})