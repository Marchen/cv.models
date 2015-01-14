#-------------------------------------------------------------------------------
#'	Modify parameters used for predict.
#'
#'	This function modifies parameters used for prediction. 
#'	This function is an internal function and not intended to be used directly 
#'	by users.
#'
#'	@inheritParams modify.args.model
#'	@return Modified args.predict list.
#'
#'	@details
#'	This function conduct following modification on parameters used for
#	prediction according to the class of the \emph{object}.
#'	\describe{
#'		\item{default}{
#'			By default, this function set \emph{type} in \emph{args.predict} to
#'			\emph{"prob"} if class of response variable is factor. Otherwise,
#'			this function set \emph{type} to \emph{"response"}.
#'		}
#'		\item{\code{\link[tree]{tree}}, \code{\link[rpart]{rpart}}}{
#'			This function do nothing.
#'		}
#'		\item{\code{\link[randomForest]{randomForest}}}{
#'			If type of model is classification, this function set \emph{type} in
#'			\emph{args.predict} to \emph{"prob"}. Otherwise, this function set
#'			\emph{type} to \emph{"response"}.
#'		}
#'		\item{\code{\link[e1071]{svm}}}{
#'			If the class of response variable is factor, this function set
#'			\emph{probability} in \emph{args.model} to TRUE.
#'		}
#'	}
#'
#'	@return A list containing modified parameters for predictions.
#-------------------------------------------------------------------------------
#	predict‚Ég‚¤ˆø”‚ğw•WŒvZ‚Å³‚µ‚­“®‚­‚æ‚¤‚ÉC³‚·‚é‘ÌŠÖ”B
#
#	Args:
#		object: ƒ‚ƒfƒ‹ƒIƒuƒWƒFƒNƒgB
#		args.predict: predict‚É“n‚³‚ê‚éˆø”‚ª“ü‚Á‚½ƒŠƒXƒgB
#-------------------------------------------------------------------------------
modify.args.predict <- function(object, args.model, args.predict, data){
	UseMethod("modify.args.predict")
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.args.predict Default S3 method.
#'	@method modify.args.predict default
#-------------------------------------------------------------------------------
#	default‚Í‰“š•Ï”‚ªˆöqŒ^‚¾‚Á‚½‚çtype‚ğ"prob"‚ÉA‚»‚êˆÈŠO‚Ìê‡‚Í"response"‚É
#	‘‚«Š·‚¦‚éB
#	OK: glm, lm, gbm, cforest, svm, lmer, glmer, lme, randomForest
#-------------------------------------------------------------------------------
modify.args.predict.default <- function(object, args.model, args.predict, data){
	if (is(get.response.var(object, data, args.model), "factor")){
		args.predict$type <- "prob"
	} else {
		args.predict$type <- "response"
	}
	return(args.predict)
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.args.predict
#'	Method for \code{\link[tree]{tree}} class of \emph{tree} package.
#'	@method modify.args.predict tree
#-------------------------------------------------------------------------------
modify.args.predict.tree <- function(object, args.model, args.predict, data){
	return(args.predict)
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.args.predict
#'	Method for \code{\link[rpart]{rpart}} class of \emph{rpart} package.
#'	@method modify.args.predict rpart
#-------------------------------------------------------------------------------
modify.args.predict.rpart <- function(object, args.model, args.predict, data){
	return(args.predict)
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.args.predict
#'	Method for \code{\link[randomForest]{randomForest}} class of
#'	\emph{randomForest} package.
#'	@method modify.args.predict randomForest
#-------------------------------------------------------------------------------
modify.args.predict.randomForest <- function(object, args.predict, check.args){
	if (check.args){
		if (object$type == "classification"){
			args.predict$type <- "prob"		# ï¿½ï¿½ï¿½Ê–ï¿½ï¿½È‚ï¿½prob
		} else {
			args.predict$type <- "response"	# ï¿½ï¿½Aï¿½È‚ï¿½response
		}
	}
	return(args.predict)
}


#-------------------------------------------------------------------------------
#'	@describeIn modify.args.predict
#'	Method for \code{\link[e1071]{svn}} class of \emph{e1071} package.
#'	@method modify.args.predict svm
#-------------------------------------------------------------------------------
modify.args.predict.svm <- function(object, args.model, args.predict, data){
	if (is(get.response.var(object, data, args.model), "factor")){
		# ˆöqŒ^‚¾‚Á‚½‚çŠeƒNƒ‰ƒX‚ÌŠm—¦‚àŒvZ‚³‚¹‚éB
		args.predict$probability = TRUE
	}
	return(args.predict)
}


