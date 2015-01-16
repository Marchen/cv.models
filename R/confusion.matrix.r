#-------------------------------------------------------------------------------
#'	Make confusion matrix.
#'
#'	This function makes confusion matrices for given thresholds.
#'
#'	@export
#'	@param response a vector containing response variable.
#'	@param prediction 
#'		a vector containing predicted probability of each observation.
#'	@param thresholds 
#'		a vector of threshold values by which positive and negative observations
#'		were separated. Multiple thresholds are acceptable.
#'	@param positive.class
#'		a character specifing label of positive cases in response variable. If
#'		\emph{positive.class} is missing, this function automatically considers
#'		"TRUE" and "1" as positive cases if classes of the \emph{response} is 
#'		only "TRUE" and "FALSE", or "1" and "0", respectively. In other case,
#'		this function considers second class of response, i.e. 
#'		levels(as.factor(response))[2] as positive, like \code{\link[pROC]{roc}}
#'		function in \emph{pROC} package.
#'
#'	@return a list containing tables object of confusion matrices.
#-------------------------------------------------------------------------------
confusion.matrix <- function(
	response, prediction, thresholds = 0.5, positive.class = NULL
){
	# —z«‚Æ‚µ‚Äˆµ‚¤ƒNƒ‰ƒX‚ðŽæ“¾BŽæ“¾‚Å‚«‚È‚©‚Á‚½‚çlevels‚Ì‚Q”Ô–Ú‚ð—z«‚Æ‚µ‚Äˆµ‚¤B
	# pROC::coords‚Æ“¯‚¶B
	positive.class <- get.positive.class(response, positive.class)
	positive.class <- ifelse(
		is.null(positive.class), levels(as.factor(response))[2], positive.class
	)
	condition <- ifelse(response == positive.class, "positive", "negative")
	condition <- factor(condition, levels = c("positive", "negative"))
	
	result <- list()
	for (i in 1:length(thresholds)){
		test <- ifelse(prediction > thresholds[i], "positive", "negative")
		test <- factor(test, levels = c("positive", "negative"))
		result[[i]] <- table(test, condition)
		attr(result[[i]], "threshold") <- thresholds[i]
	}
	return(result)
}


