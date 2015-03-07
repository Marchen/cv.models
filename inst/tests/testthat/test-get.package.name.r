require(testthat)

#-------------------------------------------------------------------------------
test_that("Test get.package.name() can handle function name.", {
	function.names <- c(
		"lm", "glm", "lme", "lmer", "glmer", "ctree", "cforest",
		"randomForest", "gbm", "svm", "tree", "rpart", "gam", "gamm"
	)
	package.names <- c(
		"stats", "stats", "nlme", "lme4", "lme4", "party", "party",
		"randomForest", "gbm", "e1071", "tree", "rpart", "mgcv", "mgcv"
	)
	for (i in 1:length(function.names)){
		expect_equal(
			get.package.name(function.names[i]), package.names[i],
			info = sprintf(
				"function.name = %s and package.name = %s",
				function.names[i], package.names[i]
			)
		)
	}	
})

test_that("Test get.package.name() can handle model objects.",{
	
	data(iris)
	
	model <- lm(Sepal.Length ~ ., data = iris)
	expect_equal(get.package.name(model), "stats")
	
	model <- glm(Sepal.Length ~ ., data = iris)
	expect_equal(get.package.name(model), "stats")
	
	model <- nlme::lme(
		Sepal.Length ~ . - Species, random = ~ 1 | Species, data = iris
	)
	expect_equal(get.package.name(model), "nlme")
	
	model <- lme4::lmer(Sepal.Length ~ . + (1|Species), data = iris)
	expect_equal(get.package.name(model), "lme4")
	
	model <- lme4::glmer(
		Sepal.Length ~ . + (1|Species), data = iris, family = Gamma
	)
	expect_equal(get.package.name(model), "lme4")
	
	model <- party::ctree(Sepal.Length ~ ., data = iris)
	expect_equal(get.package.name(model), "party")
	
	model <- party::cforest(Sepal.Length ~ ., data = iris)
	expect_equal(get.package.name(model), "party")
	
	model <- randomForest::randomForest(Sepal.Length ~ ., data = iris)
	expect_equal(get.package.name(model), "randomForest")
	
	model <- gbm::gbm(Sepal.Length ~ ., data = iris)
	expect_equal(get.package.name(model), "gbm")
	
	model <- e1071::svm(Sepal.Length ~ ., data = iris)
	expect_equal(get.package.name(model), "e1071")
	
	model <- tree::tree(Sepal.Length ~ ., data = iris)
	expect_equal(get.package.name(model), "tree")
	
	model <- rpart::rpart(Sepal.Length ~ ., data = iris)
	expect_equal(get.package.name(model), "rpart")
	
	model <- gam::gam(Sepal.Length ~ ., data = iris)
	expect_equal(get.package.name(model), "gam")
	
	require(mgcv)
	gamm.data <- gamSim(1, n = 200, scale = 2)
	
	model <- mgcv::gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = gamm.data)
	expect_equal(get.package.name(model), "mgcv")
	
	model <- mgcv::gamm(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = gamm.data)
	expect_equal(get.package.name(model), "mgcv")
	
	detach("package:mgcv", unload=TRUE)
})

