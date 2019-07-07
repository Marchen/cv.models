set.seed.if.possible <- function(object) {
	if (!is.null(object$seed)) {
		set.seed(object$seed)
	}
}


determine.positive.class <- function(object) {
	response <- object$adapter$y.vars[[1]]
	if (is.numeric(response)) {
		return("1")
	}
	if (is.logical(response)) {
		return("TRUE")
	}
	if (!is.null(object$positive.class)) {
		return(object$positive.class)
	}
	if (is.factor(response)) {
		return(levels(response)[1])
	}
	if (is.character(response)) {
		return(levels(as.factor(response))[1])
	}
	stop(
		"Response of the model should be integer, logical, factor or character."
	)
}

calculate.model.index <- function(object, index) {
	if (is.null(object$grid.predict)) {
		return(1L)
	}
	n.grid.predict <- nrow(expand.grid(object$grid.predict))
	model.index <- (index - 1) %/% n.grid.predict + 1
	return(model.index)
}

calculate.predict.index <- function(object, index) {
	if (is.null(object$grid.predict)) {
		return(1L)
	}
	n.grid.predict <- nrow(expand.grid(object$grid.predict))
	predict.index <- (index - 1) %% n.grid.predict + 1
	return(predict.index)
}
