require(testthat)

#-------------------------------------------------------------------------------
test_that(
	"run cv.models with ranger (regression, no tuning, no cluster)",
{
	data(iris)
	cv <- cv.models(
		ranger, args.model = list(Sepal.Length ~ ., write.forest = TRUE),
		data = iris,
		cv.metrics = c("mse", "rmse", "r.squared"), n.cores = 1,
		seed = 1
	)
	print(cv)
	bm <- get.best.models(cv, metrics = c("r.squared", "mse", "rmse"))
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that(
	"run cv.models with ranger (classification, no tuning, no cluster)",
{
	data(iris)
	iris <- subset(iris, Species != "setosa")
	iris$Species <- factor(iris$Species)
	cv <- cv.models(
		ranger, args.model = list(Species ~ ., write.forest = TRUE),
		data = iris, cv.metrics = c("threshold", "auc", "informedness"),
		n.cores = 1, seed = 1
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})

##-------------------------------------------------------------------------------
#test_that(
	#"run cv.models with ranger (regression, with tuning, no cluster)",
#{
	#data(iris)
	#cv <- cv.models(
		#ranger,
		#args.model = list(
			#Sepal.Length ~ ., mtry = 1:3, sampsize = c(10, 30, 100),
			#nodesize = c(3, 5), maxnodes = c(2, 3)
		#),
		#data = iris,
		#cv.metrics = c("threshold", "auc", "mse", "rmse", "informedness"), n.cores = 1
	#)
	#print(cv)
	#bm <- get.best.models(cv)
	#print(bm)
	#summary(bm)
#})

##-------------------------------------------------------------------------------
#test_that(
	#"run cv.models with ranger (classification, with tuning, no cluster)",
#{
	#data(iris)
	#iris <- subset(iris, Species != "setosa")
	#iris$Species <- as.numeric(iris$Species) - 2
	#cv <- cv.models(
		#ranger,
		#args.model = list(
			#Species ~ ., mtry = 1:3, sampsize = c(10, 30),
			#nodesize = c(3, 5), maxnodes = c(2, 3)
		#),
		#data = iris,
		#cv.metrics = c("auc", "mse", "rmse", "informedness"), n.cores = 1
	#)
	#print(cv)
	#bm <- get.best.models(cv)
	#print(bm)
	#summary(bm)
#})


#-------------------------------------------------------------------------------
test_that(
	"run cv.models with ranger (regression, no tuning, with cluster)",
{
	data(iris)
	cv <- cv.models(
		ranger, args.model = list(Sepal.Length ~ ., write.forest = TRUE),
		data = iris, cv.metrics = c("mse", "rmse", "r.squared")
	)
	print(cv)
	bm <- get.best.models(cv, metrics = c("r.squared", "mse", "rmse"))
	print(bm)
	summary(bm)
})

#-------------------------------------------------------------------------------
test_that(
	"run cv.models with ranger (classification, no tuning, with cluster)",
{
	data(iris)
	iris <- subset(iris, Species != "setosa")
	iris$Species <- factor(iris$Species)
	cv <- cv.models(
		ranger, args.model = list(Species ~ ., write.forest = TRUE),
		data = iris, cv.metrics = c("threshold", "auc", "informedness")
	)
	print(cv)
	bm <- get.best.models(cv)
	print(bm)
	summary(bm)
})


##-------------------------------------------------------------------------------
#test_that(
	#"run cv.models with ranger (regression, with tuning, with cluster)",
#{
	#data(iris)
	#cv <- cv.models(
		#ranger,
		#args.model = list(
			#Sepal.Length ~ ., mtry = 1:3, sampsize = c(10, 30, 100),
			#nodesize = c(3, 5), maxnodes = c(2, 3)
		#),
		#data = iris,
		#cv.metrics = c("auc", "mse", "rmse", "informedness")
	#)
	#print(cv)
	#bm <- get.best.models(cv)
	#print(bm)
	#summary(bm)
#})

##-------------------------------------------------------------------------------
#test_that(
	#"run cv.models with ranger (classification, with tuning, with cluster)",
#{
	#data(iris)
	#iris <- subset(iris, Species != "setosa")
	#iris$Species <- as.numeric(iris$Species) - 2
	#cv <- cv.models(
		#ranger,
		#args.model = list(
			#Species ~ ., mtry = 1:3, sampsize = c(10, 30),
			#nodesize = c(3, 5), maxnodes = c(2, 3)
		#),
		#data = iris,
		#cv.metrics = c("auc", "mse", "rmse", "informedness")
	#)
	#print(cv)
	#bm <- get.best.models(cv)
	#print(bm)
	#summary(bm)
#})

#detach("package:ranger", unload = TRUE)

