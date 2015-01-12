require(test_that)

test.data <- read.csv(
	"C:/Users/mic/Dropbox/おしごと/かいせき/2013.05.08 いまひたん/Data/imahi.analyze.csv"
)
test.data2 <-test.data
test.data2$dead <- as.factor(test.data2$dead)


#-------------------------------------------------------------------------------
test_that("randomForest、識別、チューニングなし", {
	cvrf <- cv.models(
		randomForest,
		args.model = list(
			formula = dead ~ ba + ba.mizunara + ba.konara + ba.buna + ba.sugi,
			ntrees = 2000
		),
		data = test.data2, cv.metrics = c("auc", "mse", "rmse", "informedness"),
		n.cores = 4, seed = 1
	)
	cvrf
	r <- get.best.models(cvrf)
	summary(r)
})

#-------------------------------------------------------------------------------
test_that("randomForest、回帰、チューニングなし", {
	cvrf <- cv.models(
		randomForest,
		args.model = list(
			formula = dead ~ ba + ba.mizunara + ba.konara + ba.buna + ba.sugi,
			ntrees = 2000
		),
		data = test.data, cv.metrics = c("auc", "mse", "rmse", "informedness"),
		n.cores = 4, seed = 1
	)
	cvrf
	r <- get.best.models(cvrf)
	summary(r)
})

#-------------------------------------------------------------------------------
test_that("randomForest、回帰、チューニングあり", {
	cvrf <- cv.models(
		randomForest,
		args.model = list(
			formula = n.mizunara ~ ba + ba.mizunara + ba.konara + ba.buna + ba.sugi,
			mtry = 1:5
		),
		data = test.data, cv.metrics = c("auc", "mse", "rmse", "informedness"),
		n.cores = 4
	)
	cvrf
	r <- get.best.models(cvrf, metrics = "mse")
	summary(r)
})

