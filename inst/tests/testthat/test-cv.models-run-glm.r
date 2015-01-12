require(testthat)

test.data <- read.csv(
	"C:/Users/mic/Dropbox/‚¨‚µ‚²‚Æ/‚©‚¢‚¹‚«/2013.05.08 ‚¢‚Ü‚Ğ‚½‚ñ/Data/imahi.analyze.csv"
)
test.data2 <-test.data
test.data2$dead <- as.factor(test.data2$dead)


#-------------------------------------------------------------------------------
test_that("GLM, binomial", {
	cvglm <- cv.models(
		glm,
		args.model = list(formula = dead ~ ba + ba.mizunara, family = binomial),
		data = test.data, cv.metrics = c("auc", "mse", "rmse", "informedness"),
		n.cores = 1
	)
	cvglm
	r <- get.best.models(cvglm)
	r
	summary(r)
})

#-------------------------------------------------------------------------------
test_that("GLM, poisson", {
	cvglm <- cv.models(
		glm,
		args.model = list(
			formula = n.mizunara ~ ba + ba.mizunara, family = poisson
		),
		data = test.data, cv.metrics = c("auc", "mse", "rmse", "informedness"),
		n.cores = 1
	)
	cvglm
	r <- get.best.models(cvglm)
	r
	summary(r)
})

