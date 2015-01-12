require(testthat)

test.data <- read.csv(
	"C:/Users/mic/Dropbox/おしごと/かいせき/2013.05.08 いまひたん/Data/imahi.analyze.csv"
)
test.data2 <-test.data
test.data2$dead <- as.factor(test.data2$dead)

#-------------------------------------------------------------------------------
test_that("tree、回帰問題", {
	cvtree <- cv.models(
		tree, args.model = list(
			formula = dead ~ ba.mizunara+ba.konara+ba.buna+ba.sugi
		),
		data = test.data, cv.metrics = c("auc", "mse", "rmse", "informedness"),
		n.cores = 1
	)
	cvtree
	r <- get.best.models(cvtree)
	r
	summary(r)
})

#-------------------------------------------------------------------------------
test_that("ctree、識別問題", {
	cvtree <- cv.models(
		tree, args.model = list(
			formula = dead ~ ba.mizunara + ba.konara + ba.buna + ba.sugi
		),
		data = test.data2,
		cv.metrics = c("auc", "mse", "rmse", "informedness", "threshold"),
		n.cores = 1
	)
	cvtree
	r <- get.best.models(cvtree)
	r
	summary(r)
})