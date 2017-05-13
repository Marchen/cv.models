test.settings.glm <- function(type = c("regression", "classification")){
	type <- match.arg(type)
	data(iris)
	if (type == "regression"){
		r <- model.settings(
			function.name = "glm", "package.name" = "stats",
			args.model = list(Sepal.Length ~ ., family = gaussian),
			data = iris
		)
	} else {
		iris <- subset(iris, Species != "setosa")
		iris$Species <- as.numeric(iris$Species) - 2
		r <- model.settings(
			function.name = "glm", "package.name" = "stats",
			args.model = list(Species ~ Petal.Length, family = binomial),
			data = iris
		)
	}
	return(r)
}

test.adapter.glm <- function(type){
	return(model.adapter(test.settings.glm(type)))
}



test.settings.lm <- function(){
	data(iris)
	r <- model.settings(
		function.name = "lm", "package.name" = "stats",
		args.model = list(Sepal.Length ~ ., family = gaussian), data = iris
	)
	return(r)
}

test.adapter.lm <- function(){
	return(model.adapter(test.settings.lm()))
}


