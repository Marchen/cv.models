#==============================================================================
#	Functions used for MESS.
#==============================================================================

#------------------------------------------------------------------------------
#'	(Internal) Calculate distance between test and training data using MESS
#'
#'	This function calculate multivariate environmental similarity surface
#'	(MESS, Elith, Kearney and Phillips 2010) for each point in test data.
#'
#'	@param data.train a data.frame containing training data.
#'	@param data.test a data.frame containing test data.
#'	@param na.rm a logical indicating ignorance of NA.
#'
#'	@section References:
#'	Elith, J., M. Kearney, and S. Phillips. 2010.
#'	The art of modelling range-shifting species.
#'	Methods in Ecology and Evolution 1:330-342.
#------------------------------------------------------------------------------
calculate.mess <- function(data.train, data.test, na.rm = TRUE) {
	# Prepare matrix for the result.
	numeric.columns <- colnames(data.test)[sapply(data.test, is.numeric)]
	similarity <- matrix(
		NA, nrow = nrow(data.test), ncol = length(numeric.columns)
	)
	colnames(similarity) <- numeric.columns
	# Iterate over numeric columns.
	for (i in numeric.columns) {
		# min_i
		min.i <- min(data.train[[i]], na.rm = na.rm)
		# max_i
		max.i <- max(data.train[[i]], na.rm = na.rm)
		# f_i for all p_i
		smaller.fraction <- sapply(
			data.test[[i]],
			function(x) mean(data.train[[i]] < x, na.rm = na.rm) * 100
		)
		# Similarity: f_i = 0
		case_0 <- (data.test[[i]] - min.i) / (max.i - min.i) * 100
		# Similarity: 0 < f_i <= 50
		case_0_50 <- 2 * smaller.fraction
		# Similarity: 50 <= f_i < 100
		case_50_100 <- 2 * (100 - smaller.fraction)
		# Similarity: f_i = 100
		case_100 <- (max.i - data.test[[i]]) / (max.i - min.i) * 100
		similarity[, i] <- ifelse(
			smaller.fraction == 0, case_0,
			ifelse(
				smaller.fraction <= 50, case_0_50,
				ifelse(smaller.fraction < 100, case_50_100, case_100)
			)
		)
	}
	return(apply(similarity, 1, min))
}


#------------------------------------------------------------------------------
#'	Create a function calculate MESS of a group.
#'
#'	This function creates a function which calculate aggregated MESS for a
#'	group of points.
#'
#'	@param columns
#'		names of columns or index of columns used for calculation of MESS.
#'	@param aggregate.fun
#'		a function used for aggregation of values of MESS of a group.
#'		Functions like \code{mean}, \code{median}, \code{max} and \code{min}
#'		as well as other functions which accept vector of values and return
#'		single value can be used.
#'	@param na.rm
#'		logical indicating whether NA values should be ignored during
#'		calculation of MESS.
#'	@param ...
#'		other arguments passed for \code{aggregate.fun}.
#'
#'	@export
#------------------------------------------------------------------------------
mess <- function(columns = NULL, aggregate.fun = mean, na.rm = TRUE, ...) {
	f <- function(data.train, data.test) {
		if (!is.null(columns)) {
			data.train <- data.train[columns]
			data.test <- data.test[columns]
		}
		return(
			aggregate.fun(calculate.mess(data.train, data.test, na.rm), ...)
		)
	}
	return(f)
}


#==============================================================================
#	Functions and data used for geographic distance.
#==============================================================================

#------------------------------------------------------------------------------
#'	(Internal) Parameters for several earth ellipsoids.
#------------------------------------------------------------------------------
ELLIPSOIDS = list(
    grs80 = list(
        semi.major.axis.m = 6378137,
        semi.minor.axis.m = 6356752.3141403561,
        inverse.flattening = 298.25722210100002
	),
    wgs84 = list(
        semi.major.axis.m = 6378137,
        semi.minor.axis.m = 6356752.3142451793,
        inverse.flattening = 298.25722356300003
	),
    bessel1841 = list(
        semi.major.axis.m = 6377397.1550000003,
        semi.minor.axis.m = 6356078.9628181886,
        inverse.flattening = 299.15281279999999
	)
)


#------------------------------------------------------------------------------
#'	(Internal) Convert degree to radian.
#'
#'	@param x values in degree.
#------------------------------------------------------------------------------
deg2rad <- function(x) {
	return(x / 180 * pi)
}


#------------------------------------------------------------------------------
#'	(Internal) Find parameters for the Vincenty's formulae.
#'
#'	@param f
#'		flattening of the ellipsoid.
#'	@param L
#'		difference in longitude of two points.
#'	@param U1
#'		reduced latitude (latitude on the auxiliary sphere) of point 1.
#'	@param U2
#'		reduced latitude (latitude on the auxiliary sphere) of point 2.
#'	@param threshold
#'		threshold difference to determine convergence.
#'		The value of 1e-12 produces precision of 0.06mm.
#'	@param maxit
#'		maximum number of iteration to test convergence.
#------------------------------------------------------------------------------
find.vincenty.parameters <- function(f, L, U1, U2, threshold, maxit) {
	lambda.prev <- lambda <- L
	for (i in 1:maxit) {
		sin.sigma <- sqrt(
			(cos(U2) * sin(lambda)) ^ 2
			+ (cos(U1) * sin(U2) - sin(U1) * cos(U2) * cos(lambda)) ^ 2
		)
		cos.sigma <- sin(U1) * sin(U2) + cos(U1) * cos(U2) * cos(lambda)
		sigma <- atan2(sin.sigma, cos.sigma)
		sin.alpha <- cos(U1) * cos(U2) * sin(lambda) / sin.sigma
		cos2.alpha <- 1 - sin.alpha ^ 2
		cos.2sigma.m <- cos.sigma - (2 * sin(U1) * sin(U2) / cos2.alpha)
		C <- f / 16 * cos2.alpha * (4 + f * (4 - 3 * cos2.alpha))
		lambda <- (
			L + (1 - C) * f * sin.alpha * (
				sigma + C * sin.sigma * (
					cos.2sigma.m + C * cos.sigma * (
						-1 + 2 * (cos.2sigma.m ^ 2)
					)
				)
			)
		)
		if (all(abs(lambda - lambda.prev) <= threshold)) {
			break
		}
		lambda.prev <- lambda
	}
	if (i == maxit) {
		stop(
			"Couldn't determine distance(s).\n",
			"Try increasing the value of 'maxit'."
		)
	}
	result <- list(
		sigma = sigma, sin.sigma = sin.sigma, cos.sigma = cos.sigma,
		cos2.alpha = cos2.alpha, cos.2sigma.m = cos.2sigma.m
	)
	return(result)
}


#------------------------------------------------------------------------------
#'	(Internal) Calculate distance using the Vincenty's formulae.
#'
#'	@param a
#'		semi major axis length in m.
#'	@param b
#'		semi minor axis length in m.
#'	@param lon1
#'		vector of longitude (x) of point 1 in radian.
#'	@param phi1
#'		vector latitude (y) of point 1 in radian.
#'	@param lon2
#'		vector of longitude (x) of point 2 in radian.
#'	@param phi2
#'		vector of latitude (y) of point 2 in radian.
#'	@param threshold
#'		threshold difference to determine convergence.
#'		The value of 1e-12 produces precision of 0.06mm.
#'	@param maxit
#'		maximum number of iteration to try before convergence.
#'
#'	@section References:
#'		Vincenty, T. (1975)
#'		Direct and inverse solutions of geodesics on the ellipsoid with
#'		application of nested equations. Survey Review 23:88-93.
#------------------------------------------------------------------------------
vincenty.distance <- function(
	a, b, lon1, phi1, lon2, phi2, threshold = 1e-12, maxit = 1000
) {
	# Prepare parameters.
	f <- (a - b) / a
	U1 <- atan((1 - f) * tan(phi1))
	U2 <- atan((1 - f) * tan(phi2))
	L <- lon2 - lon1
	# Find the Vincenty's formulae's parameters.
	params <- find.vincenty.parameters(f, L, U1, U2, threshold, maxit)
	# Calculate distances.
	mu2 <- params$cos2.alpha * (a ^ 2 - b ^ 2) / b ^ 2
	A <- 1 + mu2 / 16384 * (4096 + mu2 * (-768 + mu2 * (320 - 175 * mu2)))
	B <- mu2 / 1024 * (256 + mu2 * (- 128 + mu2 * (74 - 47 * mu2)))
	delta.sigma <- B * params$sin.sigma * (
		params$cos.2sigma.m
		+ B / 4 * (
			params$cos.sigma * (-1 + 2 * params$cos.2sigma.m ^ 2)
			- B / 6 * (
				params$cos.2sigma.m * (-3 + 4 * params$sin.sigma ^ 2)
				* (-3 + 4 * params$cos.2sigma.m ^ 2)
			)
		)
	)
	s <- b * A * (params$sigma - delta.sigma)
	return(s)
}


#------------------------------------------------------------------------------
#'	Create a function calculates geographic distance.
#'
#'	@param x.name
#'		column name of longitude.
#'		Data should be stored in decimal degree.
#'	@param y.name
#'		column name of latitutde.
#'		Data should be stored in decimal degree.
#'	@param ellipsoid
#'		name of earth ellipsoids, can be "grs80", "wgs84" or "bessel1841".
#'	@param method
#'		the method used for calculation of distance.
#'		Currently, only the method using the Vincenty's formulae is
#'		implimented.
#'
#'	@details:
#'		https://en.wikipedia.org/wiki/Vincenty%27s_formulae
#'
#'	@export
#------------------------------------------------------------------------------
geographic.distance <- function(
	x.name, y.name, ellipsoid = c("grs80", "wgs84", "bessel1841"),
	method = c("vincenty"), threhsold = 1e-12, maxit = 10000
) {
	ellipsoid <- match.arg(ellipsoid)
	a <- ELLIPSOIDS[[ellipsoid]]$semi.major.axis.m
	b <- ELLIPSOIDS[[ellipsoid]]$semi.minor.axis.m
	fun <- function(data.train, data.test, na.rm = TRUE) {
		phi1 <- deg2rad(mean(data.train[y.name], na.rm = na.rm))
		phi2 <- deg2rad(mean(data.test[y.name], na.rm = na.rm))
		lon1 <- deg2rad(mean(data.train[x.name], na.rm = na.rm))
		lon2 <- deg2rad(mean(data.test[x.name], na.rm = na.rm))
		d <- vincenty.distance(a, b, lon1, phi1, lon2, phi2, threshold, maxit)
		return(d)
	}
	return(fun)
}


#==============================================================================
#	Functions used for forecast horizon.
#==============================================================================

#------------------------------------------------------------------------------
#'	(Internal) Calculate distance between training and test datasets.
#'
#'	@param fold.index
#'		an integer representing index of the fold for which the distance
#'		between test and training datasets is calculated.
#'	@param data
#'		a data.frame containing explanatory variable(s).
#'	@param cv.group
#'		a vector of integer representing grouping of observation.
#'	@param distance.fun
#'		a function calculating distance between test and training datasets.
#'		It should accept two data.frames in the following form and calculate
#'		distance between them: \code{distance.fun(training.data, test.data)}.
#'		Functions produced by \code{\link{geographic.distance}} and
#'		\code{\link{mess}} can be used.
#------------------------------------------------------------------------------
calculate.distance.between.train.and.test <- function(
	fold.index, data, cv.group, distance.fun
) {
	data.train <- data[cv.group != fold.index, , drop = FALSE]
	data.test <- data[cv.group == fold.index, , drop = FALSE]
	return(distance.fun(data.train, data.test))
}


#------------------------------------------------------------------------------
#' (Experimental) Draw a forecast horizon graph.
#'
#'	Draw a forecast horizon graph using result of the cross validation.
#'	This function is experimental.
#'
#'	@param object
#'		a \code{cv.models} object.
#'	@param metric.name
#'		a character representing name of metrics to be drawn.
#'	@param distance.fun
#'		a function which calculates distance between training and test dataset.
#'		It should accept two data.frames in the following form and calculate
#'		distance between them: \code{distance.fun(training.data, test.data)}.
#'		Functions produced by \code{\link{geographic.distance}} and
#'		\code{\link{mess}} can be used.
#'	@param index
#'		an index of candidate model in the \code{cv.models} object.
#'	@param ylab
#'		label of Y axis.
#'	@param xlab
#'		label of X axis.
#'	@param draw
#'		logical indicating whether the plot is draw.
#'		If FALSE, this function only returns calculated results and draw
#'		nothing.
#'	@param ...
#'		graphical parameters passed to plot function.
#'
#'	@returns
#'
#'	@export
#------------------------------------------------------------------------------
forecast.horizon <- function(
	object,
	metric.name = ifelse(
		object$adapter$model.type == "regression", "q.squared", "mcc"
	),
	distance.fun = mess(), index = 1, ylab = metric.name,
	xlab = deparse(substitute(distance.fun)), draw = TRUE, ...
) {
	# Extract explanatory variables.
	data <- object$adapter$data[object$adapter$x.names()]
	# Extract index.
	cv.group <- object$cv.results[[index]]$cv.group
	# Extract metric.
	object$aggregate.method = "folds"
	metrics <- cv.metrics(object, list(object$cv.results[[index]]$fits))[[1]]
	# Calculate distance between training and test data.
	distance <- sapply(
		1:object$folds, calculate.distance.between.train.and.test,
		data = data, cv.group = cv.group, distance.fun = distance.fun
	)
	plot(distance, metrics[, metric.name], ylab = ylab, xlab = xlab, ...)
	invisible(cbind(data.frame(distance = distance), metrics))
}
