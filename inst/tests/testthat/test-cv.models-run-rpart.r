
test.data <- read.csv(
	"C:/Users/mic/Dropbox/��������/��������/2013.05.08 ���܂Ђ���/Data/imahi.analyze.csv"
)
test.data2 <-test.data
test.data2$dead <- as.factor(test.data2$dead)


#-------------------------------------------------------------------------------
test_that("rpart�A����", {
	cvrpart1 <- cv.models(
		rpart, args.model = list(
			formula = dead ~ ba.mizunara+ba.konara+ba.buna+ba.sugi
		),
		data = test.data2, cv.metrics = c("auc", "mse", "rmse", "informedness"),
		n.cores = 1, seed=12
	)
	cvrpart
	r <- get.best.models(cvrpart)
	r
	summary(r)
})

#-------------------------------------------------------------------------------
test_that("rpart�A��A", {
	cvrpart2 <- cv.models(
		rpart, args.model = list(
			formula = dead ~ ba.mizunara+ba.konara+ba.buna+ba.sugi
		),
		data = test.data, cv.metrics = c("auc", "mse", "rmse", "informedness"),
		n.cores = 1, seed=12
	)
	cvrpart
	r <- get.best.models(cvrpart)
	r
	summary(r)
})

