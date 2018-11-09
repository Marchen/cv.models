#-------------------------------------------------------------------------------
#	Create grouping for cross validation.
#
#	Args:
#		object (cv.models):
#			a cv.models object having settings.
#			'seed', 'adapter', 'folds', 'stratify' and 'group' fields are used.
#-------------------------------------------------------------------------------
cv.group <- function(object) {
	# Fix random number before using random process.
	set.seed.if.possible(object)
	# Create group.
	if (!is.null(object$group)) {
		return(cv.group.from.group(object))
	}
	if (object$stratify) {
		return(cv.group.stratified(object))
	}
	return(cv.group.random(object))
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


#-------------------------------------------------------------------------------
#	Create group index from specified group.
#-------------------------------------------------------------------------------
cv.group.from.group <- function(object) {
	# Error check
	if (length(object$group) != nrow(object$adapter$data)) {
		stop("Length of 'group' should be same as the number of observation.")
	}
	# Create grouping values depending on the atomic type of 'object$group'.
	if (is.logical(object$group) | is.factor(object$group)) {
		return(as.integer(object$group))
	}
	if (is.character(object$group)) {
		return(as.integer(as.factor(object$group)))
	}
	if (is.numeric(object$group)) {
		if (all(object$group %% 1 == 0)) {
			return(as.integer(as.factor(object$group)))
		}
	}
	stop("'group' should be one of logical, factor, character or integer")
}
