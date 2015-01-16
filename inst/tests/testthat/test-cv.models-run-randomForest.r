require(testthat)

#-------------------------------------------------------------------------------
test_that(
	"run cv.models with randomForest (regression, no tuning, no cluster)",
{
	data(iris)
	cv <- cv.models(
		randomForest, args.model = list(Sepal.Length ~ .), data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness"), n.cores = 1
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that(
	"run cv.models with randomForest (classification, no tuning, no cluster)",
{
	data(iris)
	iris <- subset(iris, Species != "setosa")
	iris$Species <- as.numeric(iris$Species) - 2
	cv <- cv.models(
		randomForest, args.model = list(Species ~ .), data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness"), n.cores = 1
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that(
	"run cv.models with randomForest (regression, with tuning, no cluster)",
{
	data(iris)
	cv <- cv.models(
		randomForest,
		args.model = list(
			Sepal.Length ~ ., mtry = 1:3, sampsize = c(10, 30, 100),
			nodesize = c(3, 5), maxnodes = c(2, 3)
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
test_that(
	"run cv.models with randomForest (classification, with tuning, no cluster)",
{
	data(iris)
	iris <- subset(iris, Species != "setosa")
	iris$Species <- as.numeric(iris$Species) - 2
	cv <- cv.models(
		randomForest,
		args.model = list(
			Species ~ ., mtry = 1:3, sampsize = c(10, 30),
			nodesize = c(3, 5), maxnodes = c(2, 3)
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
test_that(
	"run cv.models with randomForest (regression, no tuning, with cluster)",
{
	data(iris)
	cv <- cv.models(
		randomForest, args.model = list(Sepal.Length ~ .), data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness")
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that(
	"run cv.models with randomForest (classification, no tuning, with cluster)",
{
	data(iris)
	iris <- subset(iris, Species != "setosa")
	iris$Species <- as.numeric(iris$Species) - 2
	cv <- cv.models(
		randomForest, args.model = list(Species ~ .), data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness")
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})


#-------------------------------------------------------------------------------
test_that(
	"run cv.models with randomForest (regression, with tuning, with cluster)",
{
	data(iris)
	cv <- cv.models(
		randomForest,
		args.model = list(
			Sepal.Length ~ ., mtry = 1:3, sampsize = c(10, 30, 100),
			nodesize = c(3, 5), maxnodes = c(2, 3)
		),
		data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness")
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that(
	"run cv.models with randomForest (classification, with tuning, with cluster)",
{
	data(iris)
	iris <- subset(iris, Species != "setosa")
	iris$Species <- as.numeric(iris$Species) - 2
	cv <- cv.models(
		randomForest,
		args.model = list(
			Species ~ ., mtry = 1:3, sampsize = c(10, 30),
			nodesize = c(3, 5), maxnodes = c(2, 3)
		),
		data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness")
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

