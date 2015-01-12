require(testthat)

test.data <- read.csv(
	"C:/Users/mic/Dropbox/��������/��������/2013.05.08 ���܂Ђ���/Data/imahi.analyze.csv"
)
test.data2 <-test.data
test.data2$dead <- as.factor(test.data2$dead)

#-------------------------------------------------------------------------------
test_that("tree�A��A���", {
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
test_that("ctree�A���ʖ��", {
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