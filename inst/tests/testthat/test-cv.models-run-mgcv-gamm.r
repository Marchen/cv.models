require(testthat)
require(mgcv)

#-------------------------------------------------------------------------------
test_that("run cv.models with mgcv::gamm (regression, no cluster)", {
	set.seed(0)
	dat <- gamSim(1, n = 200, scale = 2)
	cv <- cv.models(
		gamm, args.model = list(
			y ~ s(x0) + s(x1) + s(x2) + s(x3)
		),
 		data = dat,
		cv.metrics = c("auc", "mse", "rmse", "informedness"), n.cores = 1,
		seed = 1
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that("run cv.models with mgcv::gamm (classification, no cluster)", {
	set.seed(0)
	dat <- gamSim(1, n = 600, scale = .33, dist = "binary")
	cv <- cv.models(
		gamm, args.model = list(
			y ~ s(x0) + s(x1) + s(x2) + s(x3), family = binomial,
			niterPQL = 100
		),
 		data = dat,
		cv.metrics = c("threshold", "auc", "mse", "rmse", "informedness"),
		n.cores = 1, seed = 1
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that("run cv.models with mgcv::gamm (regression, no cluster)", {
	set.seed(0)
	dat <- gamSim(1, n = 200, scale = 2)
	cv <- cv.models(
		gamm, args.model = list(
			y ~ s(x0) + s(x1) + s(x2) + s(x3)
		),
 		data = dat,
		cv.metrics = c("auc", "mse", "rmse", "informedness"), seed = 1
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that("run cv.models with mgcv::gamm (classification, no cluster)", {
	set.seed(0)
	dat <- gamSim(1, n = 600, scale = .33, dist = "binary")
	cv <- cv.models(
		gamm, args.model = list(
			y ~ s(x0) + s(x1) + s(x2) + s(x3), family = binomial,
			niterPQL = 100
		),
 		data = dat,
		cv.metrics = c("threshold", "auc", "mse", "rmse", "informedness"),
		seed = 1
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})
