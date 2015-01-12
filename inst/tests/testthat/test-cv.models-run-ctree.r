require(testthat)

test.data <- read.csv(
	"C:/Users/mic/Dropbox/��������/��������/2013.05.08 ���܂Ђ���/Data/imahi.analyze.csv"
)
test.data2 <-test.data
test.data2$dead <- as.factor(test.data2$dead)


#-------------------------------------------------------------------------------
test_that("ctree�A��A���", {
	cvctree <- cv.models(
		ctree, args.model = list(
			formula = dead ~ ba.mizunara + ba.konara + ba.buna + ba.sugi
		),
		data = test.data, cv.metrics = c("auc", "mse", "rmse", "informedness"),
		n.cores = 1
	)
	cvctree
	r <- get.best.models(cvctree)
	r
	summary(r)
})

#-------------------------------------------------------------------------------
test_that("ctree�A���ʖ��", {
	cvctree <- cv.models(
		ctree, args.model = list(
			formula = dead ~ ba.mizunara + ba.konara + ba.buna + ba.sugi
		),
		data = test.data2,
		cv.metrics = c("auc", "mse", "rmse", "informedness", "threshold"),
		n.cores = 1
	)
	cvctree
	r <- get.best.models(cvctree)
	r
	summary(r)
})

