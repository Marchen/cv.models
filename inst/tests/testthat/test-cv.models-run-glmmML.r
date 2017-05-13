require(testthat)

#-------------------------------------------------------------------------------
test_that("run cv.models with glmmML (no cluster)", {
	id <- factor(rep(1:20, rep(5, 20)))
	y <- rbinom(100, prob = rep(runif(20), rep(5, 20)), size = 1)
	x <- rnorm(100)
	dat <- data.frame(y = y, x = x, id = id)
	cv <- cv.models(
		glmmML, args.model = args.model(y ~ x, cluster = id, family = binomial),
 		data = dat,
		cv.metrics = c("auc", "mse", "rmse", "informedness"), n.cores = 1
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that("run cv.models with glmmML (with cluster)", {
	id <- factor(rep(1:20, rep(5, 20)))
	y <- rbinom(100, prob = rep(runif(20), rep(5, 20)), size = 1)
	x <- rnorm(100)
	dat <- data.frame(y = y, x = x, id = id)
	cv <- cv.models(
		glmmML, args.model = args.model(y ~ x, cluster = id, family = poisson),
 		data = dat,
		cv.metrics = c("auc", "mse", "rmse", "informedness")
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})


detach("package:glmmML", unload = TRUE)


