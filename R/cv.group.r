#-------------------------------------------------------------------------------
#	Create grouping for cross validation.
#
#	Args:
#		object (cv.models):
#			a cv.models object having settings.
#			'seed', 'adapter', 'folds' and 'stratify' fields are used.
#-------------------------------------------------------------------------------
cv.group <- function(object) {
	# Fix random number before using random process.
	set.seed.if.possible(object)
	# Create group.
	if (object$stratify) {
		return(cv.group.stratified(object))
	} else {
		return(cv.group.random(object))
	}
}


#-------------------------------------------------------------------------------
#	Create group index by random manner.
#-------------------------------------------------------------------------------
cv.group.random <- function(object) {
	# Create groups based on row numbers.
	cv.group <- ((0:(nrow(object$adapter$data) - 1)) %% object$folds) + 1
	# Reorder them randomly.
	cv.group <- sample(cv.group, length(cv.group))
	return(cv.group)
}


#-------------------------------------------------------------------------------
#	Create group index with class stratification.
#-------------------------------------------------------------------------------
cv.group.stratified <- function(object) {
	# Check the number of response variables.
	y <- object$adapter$y.vars
	if (ncol(y) > 1) {
		stop("Currently, multiple response variables are not supported.")
	}
	y <- y[[1]]
	# Check the class of reponse variable.
	if (object$adapter$model.type == "regression") {
		message <- paste0(
			"Currently, class stratification is not supported for \n",
			"regression models."
		)
		stop(message)
	}
	# Check the minimum number of observation in a class of response variable.
	if (min(tapply(y, y, length)) < object$folds) {
		message <- paste0(
			"Number(s) of observations in a class of response variable \n",
			"is smaller than the number of 'fold'."
		)
		warning(message)
	}
	# Create groups.
	shuffled.y <- unlist(tapply(1:length(y), y, sample), use.names = FALSE)
	cv.group <- (((0:(length(y) - 1)) %% object$folds) + 1)[order(shuffled.y)]
	return(cv.group)
}
