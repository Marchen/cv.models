require(testthat)

#-------------------------------------------------------------------------------
test_that(
	"run cv.models with gbm (regression, with parameter tuning, no cluster)",
{
	data(iris)
	cv <- cv.models(
		gbm, args.model = list(
			Sepal.Length~., shrinkage = c(0.1, 0.01, 0.001),
			n.minobsinnode = c(1, 5, 10), interaction.depth = c(1, 3),
			distribution = "gaussian", n.trees = 500
		),
		args.predict = list(n.trees = seq(5, 500, by = 5)),
		data=iris, cv.metrics = c(
			"threshold", "auc", "mse", "rmse", "mcc", "informedness"
		),
		n.cores = 1
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
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
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that(
	"run cv.models with gbm (regression, with parameter tuning, with cluster)",
{
	data(iris)
	cv <- cv.models(
		gbm, args.model = list(
			Sepal.Length~., shrinkage = c(0.1, 0.01, 0.001),
			n.minobsinnode = c(1, 5, 10), interaction.depth = c(1, 3),
			distribution = "gaussian", n.trees = 500
		),
		args.predict = list(n.trees = seq(5, 500, by = 5)),
		data=iris,
		cv.metrics = c("mse", "rmse", "r.squared", "q.squared")
	)
	print(cv)
	bm <- get.best.models(cv, metrics = "r.squared")
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that(
	"run cv.models with gbm (classification, with parameter tuning, with cluster)",
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
		cv.metrics = c("threshold", "auc", "mse", "rmse", "informedness")
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that(
	"run cv.models with gbm (regression, no parameter tuning, no cluster)",
{
	data(iris)
	cv <- cv.models(
		gbm, args.model = list(Sepal.Length~., distribution = "gaussian"),
		args.predict = list(n.trees = 100),
		data=iris, cv.metrics = c(
			"threshold", "auc", "mse", "rmse", "mcc", "informedness"
		),
		n.cores = 1
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that(
	"run cv.models with gbm (classification, no parameter tuning, no cluster)",
{
	data(iris)
	iris <- subset(iris, Species != "setosa")
	iris$Species <- as.numeric(iris$Species) - 2
	cv <- cv.models(
		gbm, args.model = list(Species~., distribution = "bernoulli"),
		args.predict = list(n.trees = 100),
		data=iris, cv.metrics = c("threshold", "auc", "mse", "rmse", "informedness"),
		n.cores = 1
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that(
	"run cv.models with gbm (regression, no parameter tuning, with cluster)",
{
	data(iris)
	cv <- cv.models(
		gbm, args.model = list(Sepal.Length~., distribution = "gaussian"),
		args.predict = list(n.trees = 100),
		data=iris, cv.metrics = c(
			"threshold", "auc", "mse", "rmse", "mcc", "informedness"
		)
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that(
	"run cv.models with gbm (classification, no parameter tuning, with cluster)",
{
	data(iris)
	iris <- subset(iris, Species != "setosa")
	iris$Species <- as.numeric(iris$Species) - 2
	cv <- cv.models(
		gbm, args.model = list(Species~., distribution = "bernoulli"),
		args.predict = list(n.trees = 100),
		data=iris, cv.metrics = c("threshold", "auc", "mse", "rmse", "informedness")
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})


detach("package:gbm", unload = TRUE)



