#------------------------------------------------------------------------------
#	Define constants.
#------------------------------------------------------------------------------

# Name of parameters used when calling optimal.cutpoints.
OPTIMAL_CUTPOINT_PARAMETERS = c("methods", "op.prev", "control")

# Name conversion table between the names of metrics in this package
# and names used in the result of optimal.cutpoints.
NAME_CONVERSION_TABLE <- c(
	cutoff = "threshold", Se = "sensitivity", Sp = "specificity",
	PPV = "ppv", NPV = "npv", DLR.Positive = "dlr.positive",
	DLR.Negative = "dlr.negative", FP = "fp", FN = "fn"
)


#------------------------------------------------------------------------------
#	A reference class calculating metrics for classification models.
#------------------------------------------------------------------------------
classification.metrics.calculator <- R6Class(
	"classification.metrics.calculator",
	private = list(
		cutpoint.options = NULL,
		positive.class = NULL
	)
)


#------------------------------------------------------------------------------
#	Initialize an object from cv.models object.
#
#	Args:
#		object:
#			a cv.models object. Only fields in the fields of this class
#			are used.
#------------------------------------------------------------------------------
classification.metrics.calculator$set(
	"public", "initialize",
	function(object) {
		# Check error.
		if (is.null(object$cutpoint.options$methods)) {
			stop("'methods' should be specified in 'cutpoint.options'.")
		}
		if (length(object$cutpoint.options$methods) == 0) {
			msg = paste(
				"'methods' of 'cutpoint.options' should have at least",
				"one element."
			)
			stop(msg)
		}
		# Copy field from the object.
		private$positive.class <- determine.positive.class(object)
		private$cutpoint.options <- object$cutpoint.options
	}
)


#------------------------------------------------------------------------------
#	Get probability from the result of cross validation.
#
#	Args:
#		fit:
#			a list having result of one fold of cross validation
#			with "response" and "prediction" fields.
#------------------------------------------------------------------------------
classification.metrics.calculator$set(
	"private", "get.probability",
	function(fit) {
		return(fit$prediction[, private$positive.class])
	}
)


#------------------------------------------------------------------------------
#	Get binary response from the result of cross validation.
#
#	Args:
#		fit:
#			a list having result of one fold of cross validation
#			with "response" and "prediction" fields.
#------------------------------------------------------------------------------
classification.metrics.calculator$set(
	"private", "get.binary.response",
	function(fit) {
		return(as.numeric(fit$response == private$positive.class))
	}
)


#------------------------------------------------------------------------------
#	Prepare arguments for optimal.cutpoints.
#
#	Args:
#		fit:
#			a list having result of one fold of cross validation
#			with "response" and "prediction" fields.
#------------------------------------------------------------------------------
classification.metrics.calculator$set(
	"private", "prepare.optimal.cutpoints.args",
	function(fit) {
		# Prepare options for optimal.cutpoints().
		args <- private$cutpoint.options[OPTIMAL_CUTPOINT_PARAMETERS]
		args$X <- prediction ~ response
		args$data <- data.frame(
			response = private$get.binary.response(fit),
			prediction = private$get.probability(fit)
		)
		args$ci.fit <- FALSE
		# Because we can specify only negative class in optimal.cutpoints(),
		# using the following option to specify positive class.
		args$tag.healthy <- 0
		return(args)
	}
)


#------------------------------------------------------------------------------
#	Run optimal.cutpoints.
#
#	Args:
#		fit:
#			a list having result of one fold of cross validation
#			with "response" and "prediction" fields.
#------------------------------------------------------------------------------
classification.metrics.calculator$set(
	"private", "run.optimal.cutpoints",
	function(fit) {
		require(OptimalCutpoints)
		args <- private$prepare.optimal.cutpoints.args(fit)
		result <- do.call(optimal.cutpoints, args)
		return(result)
	}
)


#------------------------------------------------------------------------------
#	Parse result of optimal.cutpoints.
#
#	Args:
#		object:
#			an object of optimal.cutpoints.
#		method:
#			a method for which result of optimal.cutpoints is parsed.
#
#	Returns:
#		a matrix of 1 row with each column represents each metric.
#			matrix(threshold, sensitivity, ...)
#------------------------------------------------------------------------------
classification.metrics.calculator$set(
	"private", "parse.optimal.cutpoints.result",
	function(object, method) {
		# Extract threshold and metrics from the result of optimal.cutpoints.
		# Because field name "Global" is only used for the first method,
		# access element of list by index (1) instead of name ("Global").
		cutoff <- object[[method]][[1]]$optimal.cutoff
		if (any(sapply(cutoff, length) > 2)) {
			# Not sure there is a possibility to be here.
			msg <- paste0(
				"Single optimal threshold was not determined ",
				"by specified settings.\n",
				"The first threshold was used for further calculation.\n",
				"If this is a problem, change the 'cutpoint.options' option\n",
				"of the cv.models to find single optimal threshold.\n",
				"For the further information, please consult the manual of ",
				"optimal.cutpoitns."
			)
			warning(msg)
			cutoff <- lapply(cutoff, "[", 1)
		}
		# Convert the result to a matrix.
		result <- do.call(cbind, lapply(cutoff, c))
		colnames(result) <- NAME_CONVERSION_TABLE[colnames(result)]
		# Join AUC to the result.
		auc <- object[[method]][[1]]$measures.acc$AUC["AUC"]
		result <- cbind(result, auc = auc)
		return(result)
	}
)


#------------------------------------------------------------------------------
#	Calculate number of true positive (TP).
#
#	Args:
#		fit:
#			a list having result of one fold of cross validation
#			with "response" and "prediction" fields.
#		metrics:
#			result of 'parse.optimal.cutpoints.result' method.
#------------------------------------------------------------------------------
classification.metrics.calculator$set(
	"private", "calc.tp",
	function(fit, metrics) {
		n.positive <- sum(private$get.binary.response(fit))
		tp <- n.positive - metrics[, "fn"]
		return(tp)
	}
)


#------------------------------------------------------------------------------
#	Calculate number of true negative (TN).
#
#	Args:
#		fit:
#			a list having result of one fold of cross validation
#			with "response" and "prediction" fields.
#		metrics:
#			result of 'parse.optimal.cutpoints.result' method.
#------------------------------------------------------------------------------
classification.metrics.calculator$set(
	"private", "calc.tn",
	function(fit, metrics) {
		bin <- private$get.binary.response(fit)
		n.negative <- length(bin) - sum(bin)
		tn <- n.negative - metrics[, "fp"]
		return(tn)
	}
)


#------------------------------------------------------------------------------
#	Calculate accuracy.
#
#	Args:
#		metrics:
#			result of 'parse.optimal.cutpoints.result' method.
#------------------------------------------------------------------------------
classification.metrics.calculator$set(
	"private", "calc.accuracy",
	function(metrics) {
		tp <- metrics[, "tp"]
		tn <- metrics[, "tn"]
		fp <- metrics[, "fp"]
		fn <- metrics[, "fn"]
		accuracy <- (tp + tn) / (tp + tn + fp + fn)
		return(accuracy)
	}
)


#------------------------------------------------------------------------------
#	Calculate informedness.
#
#	Args:
#		metrics:
#			result of 'parse.optimal.cutpoints.result' method.
#------------------------------------------------------------------------------
classification.metrics.calculator$set(
	"private", "calc.informedness",
	function(metrics) {
		return(metrics[, "sensitivity"] + metrics[, "specificity"] - 1)
	}
)


#------------------------------------------------------------------------------
#	Calculate markedness.
#
#	Args:
#		metrics:
#			result of 'parse.optimal.cutpoints.result' method.
#------------------------------------------------------------------------------
classification.metrics.calculator$set(
	"private", "calc.markedness",
	function(metrics) {
		return(metrics[, "ppv"] + metrics[, "npv"] - 1)
	}
)


#------------------------------------------------------------------------------
#	Calculate Matthew's correlation coefficient (MCC).
#
#	Args:
#		metrics:
#			result of 'parse.optimal.cutpoints.result' method.
#------------------------------------------------------------------------------
classification.metrics.calculator$set(
	"private", "calc.mcc",
	function(metrics) {
		tp <- metrics[, "tp"]
		tn <- metrics[, "tn"]
		fp <- metrics[, "fp"]
		fn <- metrics[, "fn"]
		denom <- sqrt((tp + fn) * (tp + fp) * (tn + fp) * (tn + fn))
		mcc <- (tp * tn - fp * fn) / denom
		return(mcc)
	}
)


#------------------------------------------------------------------------------
#	Calculate log-likelihood.
#
#	Original program was obtained from Lawson et al. 2014.
#	Prevalence, thresholds and the performance of presence-absence models.
#	Methods in Ecology and Evolution 5:54-64.
#
#	Args:
#		fit:
#			a list having result of one fold of cross validation
#			with "response" and "prediction" fields.
#------------------------------------------------------------------------------
classification.metrics.calculator$set(
	"private", "calc.loglik",
	function(fit) {
		p <- private$get.probability(fit)
		y <- private$get.binary.response(fit)
		loglik <- sum(log(p * y + (1 - p) * (1 - y)))
		return(loglik)
	}
)


#------------------------------------------------------------------------------
#	Calculate Likelihood based R squared.
#
#	Original program was obtained from Lawson et al. 2014.
#	Prevalence, thresholds and the performance of presence-absence models.
#	Methods in Ecology and Evolution 5:54-64.
#
#	Args:
#		fit:
#			a list having result of one fold of cross validation
#			with "response" and "prediction" fields.
#------------------------------------------------------------------------------
classification.metrics.calculator$set(
	"private", "calc.r.squared.loglik",
	function(fit) {
		p <- private$get.probability(fit)
		y <- private$get.binary.response(fit)
		loglik.p <- sum(log(p * y + (1 - p) * (1 - y)))
		loglik.n <- sum(log(mean(p) * y + (1 - mean(p)) * (1 - y)))
		rsq <- (loglik.p - loglik.n) / (1 - loglik.n)
		return(rsq)
	}
)


#------------------------------------------------------------------------------
#	Calculate Cohen's Kappa statistic.
#
#	https://en.wikipedia.org/wiki/Cohen%27s_kappa
#
#						response
#						TRUE	FALSE
#	predicted	TRUE	tp: a	fp: b
#				FALSE	fn: c	fn: d
#
#	Args:
#		metrics:
#			result of 'parse.optimal.cutpoints.result' method.
#------------------------------------------------------------------------------
classification.metrics.calculator$set(
	"private", "calc.kappa",
	function(metrics) {
		tp <- metrics[, "tp"]
		tn <- metrics[, "tn"]
		fp <- metrics[, "fp"]
		fn <- metrics[, "fn"]
		n <- tp + tn + fp + fn
		po = (tp + tn) / n
		pe.true = (tp + fp) / n * (tp + fn) / n
		pe.false = (fn + tn) / n * (fp + tn) / n
		pe = pe.true + pe.false
		kappa = (po - pe) / (1 - pe)
		return(kappa)
	}
)


#------------------------------------------------------------------------------
#	Calculate all metrics for single method of optimal.cutpoints.
#
#	Args:
#		method:
#			a method by which result is calculated.
#		object:
#			cv.models object.
#		fit:
#			a list having result of one fold of cross validation
#			with "response", "prediction" and "index" fields.
#
#	Returns:
#		a matrix of 1 row with each column represents each metric.
#			matrix(threshold, sensitivity, ...)
#------------------------------------------------------------------------------
classification.metrics.calculator$set(
	"private", "calculate.metrics.for.single.method",
	function(method, object, fit) {
		# Calculate metrics using optimal.cutpoints.
		metrics <- private$parse.optimal.cutpoints.result(object, method)
		# Calculate TP and TN first.
		metrics <- cbind(
			metrics,
			tp = private$calc.tp(fit, metrics),
			tn = private$calc.tn(fit, metrics)
		)
		# Calculate other metrics including that depend on TP and TN.
		metrics <- cbind(
			metrics,
			accuracy = private$calc.accuracy(metrics),
			informedness = private$calc.informedness(metrics),
			markedness = private$calc.markedness(metrics),
			mcc = private$calc.mcc(metrics),
			loglik = private$calc.loglik(fit),
			rsq.loglik = private$calc.r.squared.loglik(fit),
			kappa = private$calc.kappa(metrics)
		)
		rownames(metrics) <- NULL
		return(metrics)
	}
)


#------------------------------------------------------------------------------
#	Calculate all metrics.
#
#	Args:
#		fit:
#			a list having result of one fold of cross validation
#			with "response", "prediction" and "index" fields.
#
#	Returns:
#		list(
#			method1 = matrix(threshold, sensitivity, ...),
#			method2 = matrix(threshold, sensitivity, ...),
#			...
#		)
#------------------------------------------------------------------------------
classification.metrics.calculator$set(
	"public", "calculate.metrics",
	function(fit) {
		oc.result <- private$run.optimal.cutpoints(fit)
		methods <- private$cutpoint.options$methods
		metrics <- lapply(
			methods, private$calculate.metrics.for.single.method,
			oc.result, fit
		)
		names(metrics) <- private$cutpoint.options$methods
		return(metrics)
	}
)
