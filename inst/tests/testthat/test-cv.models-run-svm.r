require(testthat)

test.data <- read.csv(
	"C:/Users/mic/Dropbox/��������/��������/2013.05.08 ���܂Ђ���/Data/imahi.analyze.csv"
)
test.data2 <-test.data
test.data2$dead <- as.factor(test.data2$dead)

#-------------------------------------------------------------------------------
test_that("svm�A���ޖ�肪���Ȃ��������H", {
	cvsvm <- cv.models(
		svm, args.model = list(formula = dead ~ ba + n.mizunara + n.konara),
		data = test.data2, cv.metrics = c("auc", "mse", "rmse", "informedness"),
		n.cores = 1
	)
	cvsvm
	r <- get.best.models(cvsvm)
	r
	summary(r)
})

#-------------------------------------------------------------------------------
test_that("svm�A��A���A�N���X�^�[�����Ȃ��������H", {
	cvsvm <- cv.models(
		svm, args.model = list(formula = n.mizunara ~ ba),
		data = test.data2, cv.metrics = c("auc", "mse", "rmse", "informedness"),
		n.cores = 4
	)
	cvsvm
	r <- get.best.models(cvsvm)
	r
	summary(r)
})

