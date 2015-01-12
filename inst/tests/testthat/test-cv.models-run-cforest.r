require(testthat)

test.data <- read.csv(
	"C:/Users/mic/Dropbox/おしごと/かいせき/2013.05.08 いまひたん/Data/imahi.analyze.csv"
)

test.data2 <-test.data
test.data2$dead <- as.factor(test.data2$dead)

test_that("cforest、識別問題、クラスター", {
	cvcforest <- cv.models(
		cforest,
		args.model = list(
			formula = dead ~ ba + ba.mizunara + ba.konara + ba.buna + ba.sugi
		),
		data = test.data2, cv.metrics = c("auc", "mse", "rmse", "informedness"),
		n.cores = 1
	)
	cvcforest
	r <- get.best.models(cvcforest)
	r
	summary(r)
})

test_that("cforest、回帰問題、クラスター", {
	cvcforest <- cv.models(
		cforest,
		args.model = list(
			formula = n.mizunara ~ ba + ba.mizunara + ba.konara + ba.buna + ba.sugi
		),
		data = test.data2, cv.metrics = c("auc", "mse", "rmse", "informedness"),
		n.cores = 4
	)
	cvcforest
	r <- get.best.models(cvcforest)
	r
	summary(r)
})

