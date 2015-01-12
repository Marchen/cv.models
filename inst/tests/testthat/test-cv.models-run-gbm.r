require(testthat)

test.data <- read.csv(
	"C:/Users/mic/Dropbox/��������/��������/2013.05.08 ���܂Ђ���/Data/imahi.analyze.csv"
)
test.data2 <-test.data
test.data2$dead <- as.factor(test.data2$dead)

#-------------------------------------------------------------------------------
test_that("gbm�A�^�C����A���f���I������A���ޖ��", {
	cvgbm <- cv.models(
		gbm, args.model = list(
			formula = dead ~ ba + ba.mizunara, distribution = "bernoulli",
			shrinkage = c(0.1, 0.01),
			interaction.depth = c(1, 3),
			n.minobsinnode = c(5, 10),
			bag.fraction = c(0.5, 0.8)
		),
		data = test.data[1:50, ],
		cv.metrics = c("auc", "mse", "rmse", "informedness"),
		args.predict = list(n.trees = c(1, 10, 100)), seed = 1,
		n.cores = 1
	)
	cvgbm
	r <- get.best.models(cvgbm)
	r
	summary(r)
})

#-------------------------------------------------------------------------------
test_that("gbm�A�^�C�Ȃ��A���f���I������A���ޖ��", {
	cvgbm <- cv.models(
		gbm, args.model = list(
			formula = dead ~ ba + ba.mizunara, distribution = "bernoulli",
			shrinkage = c(0.1, 0.01),
			interaction.depth = c(1, 3),
			n.minobsinnode = c(5, 10),
			bag.fraction = c(0.5, 0.8)
		),
		data = test.data, cv.metrics = c("auc", "mse", "rmse", "informedness"),
		args.predict = list(n.trees = c(1, 10, 100)), seed = 1,
		n.cores = 1
	)
	cvgbm
	r <- get.best.models(cvgbm)
	r
	summary(r)
})

#-------------------------------------------------------------------------------
test_that("gbm�A�^�C����A���f���I���Ȃ��A���ޖ��", {
	cvgbm <- cv.models(
		gbm, args.model = list(
			formula = dead ~ ba + ba.mizunara, distribution = "bernoulli"
		),
		data = test.data[1:50, ],
		cv.metrics = c("auc", "mse", "rmse", "informedness"),
		args.predict = list(n.trees = 100), seed = 1,
		n.cores = 1
	)
	cvgbm
	r <- get.best.models(cvgbm)
	r
	summary(r)
})

#-------------------------------------------------------------------------------
test_that("gbm�A�^�C�Ȃ��A���f���I���Ȃ��A���ޖ��", {
	cvgbm <- cv.models(
		gbm, args.model = list(
			formula = dead ~ ba + ba.mizunara, distribution = "bernoulli"
		),
		data = test.data, cv.metrics = c("auc", "mse", "rmse", "informedness"),
		args.predict = list(n.trees = 100), seed = 1,
		n.cores = 1
	)
	cvgbm
	r <- get.best.models(cvgbm)
	r
	summary(r)
})

test_that("gbm�A�^�C����A���f���I���Ȃ��A��A���", {
	cvgbm <- cv.models(
		gbm, args.model = list(
			formula = n.mizunara ~ ba + ba.mizunara, distribution = "gaussian"
		),
		data = test.data[1:50, ],
		cv.metrics = c("auc", "mse", "rmse", "informedness"),
		args.predict = list(n.trees = c(1,10,100)), seed = 1,
		n.cores = 1
	)
	cvgbm
	r <- get.best.models(cvgbm)
	r
	summary(r)
})

