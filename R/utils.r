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
