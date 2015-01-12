require(testthat)

test.data <- read.csv(
	"C:/Users/mic/Dropbox/‚¨‚µ‚²‚Æ/‚©‚¢‚¹‚«/2013.05.08 ‚¢‚Ü‚Ğ‚½‚ñ/Data/imahi.analyze.csv"
)
test.data2 <-test.data
test.data2$dead <- as.factor(test.data2$dead)


#-------------------------------------------------------------------------------
test_that("ctreeA‰ñ‹A–â‘è", {
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
test_that("ctreeA¯•Ê–â‘è", {
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

