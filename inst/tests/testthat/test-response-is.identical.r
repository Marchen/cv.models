require(testthat)
require(mgcv)
#
#	cv.responseに入っている応答変数の値がオリジナルと同じかどうかをチェックする。
#

#-------------------------------------------------------------------------------
test_that("Test response variable in a result is correct (cforest)", {
	data(iris)
	iris <- subset(iris, Species != "setosa")
	iris$Species <- as.numeric(iris$Species) - 2
	cv <- cv.models(
		cforest, args.model = list(Species ~ .), data = iris,
		cv.metrics = c("threshold", "auc", "mse", "rmse", "informedness"),
		n.cores = 1
	)
	bm <- get.best.models(cv)
	for (i in 1:ncol(cv$cv.response)){
		expect_identical(cv$cv.response[[i]], as.factor(iris$Species))
	}
	for (i in 1:length(bm)){
		expect_identical(bm[[i]]$cv.response, as.factor(iris$Species))
	}
})

#-------------------------------------------------------------------------------
test_that("Test response variable in a result is correct (ctree)", {
	data(iris)
	iris <- subset(iris, Species != "setosa")
	iris$Species <- as.numeric(iris$Species) - 2
	cv <- cv.models(
		ctree, args.model = list(Species ~ .), data = iris,
		cv.metrics = c("threshold", "auc", "mse", "rmse", "informedness"),
		n.cores = 1,
		seed = 2
	)
	bm <- get.best.models(cv)
	for (i in 1:ncol(cv$cv.response)){
		expect_identical(cv$cv.response[[i]], as.factor(iris$Species))
	}
	for (i in 1:length(bm)){
		expect_identical(bm[[i]]$cv.response, as.factor(iris$Species))
	}
})

#-------------------------------------------------------------------------------
test_that("Test response variable in a result is correct (gam::gam)", {
	data(iris)
	iris <- subset(iris, Species != "setosa")
	iris$Species <- as.numeric(iris$Species) - 2
	cv <- cv.models(
		gam, args.model = list(Species ~ Petal.Length, family = binomial),
		data = iris,
		cv.metrics = c("threshold", "auc", "mse", "rmse", "informedness"),
		n.cores = 1, package.name = "gam"
	)
	bm <- get.best.models(cv)
	for (i in 1:ncol(cv$cv.response)){
		expect_identical(cv$cv.response[[i]], iris$Species)
	}
	for (i in 1:length(bm)){
		expect_identical(bm[[i]]$cv.response, iris$Species)
	}
})

#-------------------------------------------------------------------------------
test_that("Test response variable in a result is correct (gbm)", {
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
		data = iris,
		cv.metrics = c("threshold", "auc", "mse", "rmse", "informedness"),
		n.cores = 1
	)
	bm <- get.best.models(cv)
	for (i in 1:ncol(cv$cv.response)){
		expect_identical(cv$cv.response[[i]], iris$Species)
	}
	for (i in 1:length(bm)){
		expect_identical(bm[[i]]$cv.response, iris$Species)
	}
})

#-------------------------------------------------------------------------------
test_that("Test response variable in a result is correct (glm)", {
	data(iris)
	iris <- subset(iris, Species != "setosa")
	iris$Species <- as.numeric(iris$Species) - 2
	cv <- cv.models(
		glm, args.model = list(Species ~ Petal.Length, family = binomial),
		data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness", "threshold"),
		n.cores = 1, positive.class = "1"
	)
	bm <- get.best.models(cv)
	for (i in 1:ncol(cv$cv.response)){
		expect_identical(cv$cv.response[[i]], iris$Species)
	}
	for (i in 1:length(bm)){
		expect_identical(bm[[i]]$cv.response, iris$Species)
	}
})

#-------------------------------------------------------------------------------
test_that("Test response variable in a result is correct (glmer)", {
	data(iris)
	iris <- subset(iris, Species != "setosa")
	iris$Species <- as.numeric(iris$Species) - 2
	iris$Random <- c(rep(1:5, 10), rep(6:10, 10))
	cv <- cv.models(
		glmer, args.model = list(
			Species ~ Sepal.Length + (1|Random), family = binomial
		),
 		data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness"), n.cores = 1
	)
	bm <- get.best.models(cv)
	for (i in 1:ncol(cv$cv.response)){
		expect_identical(cv$cv.response[[i]], iris$Species)
	}
	for (i in 1:length(bm)){
		expect_identical(bm[[i]]$cv.response, iris$Species)
	}
})

#-------------------------------------------------------------------------------
test_that("Test response variable in a result is correct (lm)", {
	data(iris)
	cv <- cv.models(
		lm, args.model = list(Sepal.Length ~ .), data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness"), n.cores = 1
	)
	bm <- get.best.models(cv)
	for (i in 1:ncol(cv$cv.response)){
		expect_identical(cv$cv.response[[i]], iris$Sepal.Length)
	}
	for (i in 1:length(bm)){
		expect_identical(bm[[i]]$cv.response, iris$Sepal.Length)
	}
})

#-------------------------------------------------------------------------------
test_that("Test response variable in a result is correct (lme)", {
	data(iris)
	cv <- cv.models(
		lme, args.model = list(
			Sepal.Length ~ . - Species, random = ~ 1 | Species
		),
 		data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness"), n.cores = 1
	)
	bm <- get.best.models(cv)
	for (i in 1:ncol(cv$cv.response)){
		expect_identical(cv$cv.response[[i]], iris$Sepal.Length)
	}
	for (i in 1:length(bm)){
		expect_identical(bm[[i]]$cv.response, iris$Sepal.Length)
	}
})

#-------------------------------------------------------------------------------
test_that("Test response variable in a result is correct (lmer)", {
	data(iris)
	cv <- cv.models(
		lmer, args.model = list(Sepal.Length ~ . + (1|Species)),
 		data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness"), n.cores = 1
	)
	bm <- get.best.models(cv)
	for (i in 1:ncol(cv$cv.response)){
		expect_identical(cv$cv.response[[i]], iris$Sepal.Length)
	}
	for (i in 1:length(bm)){
		expect_identical(bm[[i]]$cv.response, iris$Sepal.Length)
	}
})

#-------------------------------------------------------------------------------
test_that("Test response variable in a result is correct (mgcv::gam)", {
	set.seed(0)
	dat <- gamSim(1, n = 200, scale = 2)
	cv <- cv.models(
		gam, args.model = list(
			y ~ s(x0) + s(x1) + s(x2) + s(x3)
		),
 		data = dat,
		cv.metrics = c("auc", "mse", "rmse", "informedness"), n.cores = 1,
		seed = 1
	)
	bm <- get.best.models(cv)
	for (i in 1:ncol(cv$cv.response)){
		expect_identical(cv$cv.response[[i]], dat$y)
	}
	for (i in 1:length(bm)){
		expect_identical(bm[[i]]$cv.response, dat$y)
	}
})

#-------------------------------------------------------------------------------
test_that("Test response variable in a result is correct (mgcv::gamm)", {
	set.seed(0)
	dat <- gamSim(1, n = 200, scale = 2)
	cv <- cv.models(
		gamm, args.model = list(
			y ~ s(x0) + s(x1) + s(x2) + s(x3)
		),
 		data = dat,
		cv.metrics = c("auc", "mse", "rmse", "informedness"), n.cores = 1,
		seed = 1
	)
	bm <- get.best.models(cv)
	for (i in 1:ncol(cv$cv.response)){
		expect_identical(cv$cv.response[[i]], dat$y)
	}
	for (i in 1:length(bm)){
		expect_identical(bm[[i]]$cv.response, dat$y)
	}
})

#-------------------------------------------------------------------------------
test_that("Test response variable in a result is correct (randomForest)", {
	data(iris)
	iris <- subset(iris, Species != "setosa")
	iris$Species <- as.integer(iris$Species) - 2L
	cv <- cv.models(
		randomForest, args.model = list(Species ~ .), data = iris,
		cv.metrics = c("threshold", "auc", "mse", "rmse", "informedness"),
		n.cores = 1,
		seed = 1
	)
	bm <- get.best.models(cv)
	for (i in 1:ncol(cv$cv.response)){
		expect_identical(cv$cv.response[[i]], as.factor(iris$Species))
	}
	for (i in 1:length(bm)){
		expect_identical(bm[[i]]$cv.response, as.factor(iris$Species))
	}
})

#-------------------------------------------------------------------------------
test_that("Test response variable in a result is correct (rpart)", {
	data(iris)
	iris <- subset(iris, Species != "setosa")
	iris$Species <- as.numeric(iris$Species) - 2
	cv <- cv.models(
		rpart, args.model = list(Species ~ .), data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness"), n.cores = 1
	)
	bm <- get.best.models(cv)
	for (i in 1:ncol(cv$cv.response)){
		expect_identical(cv$cv.response[[i]], as.factor(iris$Species))
	}
	for (i in 1:length(bm)){
		expect_identical(bm[[i]]$cv.response, as.factor(iris$Species))
	}
})

#-------------------------------------------------------------------------------
test_that("Test response variable in a result is correct (svm)", {
	data(iris)
	cv <- cv.models(
		svm, args.model = list(Sepal.Length ~ .), data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness"), n.cores = 1
	)
	bm <- get.best.models(cv)
	for (i in 1:ncol(cv$cv.response)){
		expect_identical(cv$cv.response[[i]], iris$Sepal.Length)
	}
	for (i in 1:length(bm)){
		expect_identical(bm[[i]]$cv.response, iris$Sepal.Length)
	}
})

#-------------------------------------------------------------------------------
test_that("Test response variable in a result is correct (tree)", {
	data(iris)
	cv <- cv.models(
		tree, args.model = list(Sepal.Length ~ .), data = iris,
		cv.metrics = c("auc", "mse", "rmse", "informedness"), n.cores = 1
	)
	bm <- get.best.models(cv)
	for (i in 1:ncol(cv$cv.response)){
		expect_identical(cv$cv.response[[i]], iris$Sepal.Length)
	}
	for (i in 1:length(bm)){
		expect_identical(bm[[i]]$cv.response, iris$Sepal.Length)
	}
})

detach("package:mgcv", unload=TRUE)
