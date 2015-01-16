#-------------------------------------------------------------------------------
#'	Modify class of response variable.
#'
#'	Some functions require factor response variable for binary classification.
#'	This function change the class of response variable to attain correct result. 
#'	This function is an internal function and not intended to be used directly 
#'	by users.
#'
#'	@inheritParams modify.args.model
#'	@param response The response variable to be modified.
#'
#'	@details
#'	If the object is a object of \code{\link[tree]{tree}},
#'	\code{\link[rpart]{rpart}}, \code{\link[randomForest]{randomForest}} or
#'	\code{\link[e1071]{svm}} and unique values of response variable are (TRUE
#'	and FALSE) or (0 and 1), this function change the class of response variable
#'	to factor.
#'
#'	@return A vector containing modified/unmodified response variable
#-------------------------------------------------------------------------------
#	応答変数がTRUE/FALSEだったり、0/1の二値だったりしたとき、因子型として与えない
#	と識別問題として扱わないモデルのため、応答変数の型を変換する総称関数。
#	tree, rpart, randomForest, svm関数には変換処理をする。
#
#	Args:
#		object: モデルオブジェクト。ダミーでOK。
#		response: 修正する応答変数。
#-------------------------------------------------------------------------------
modify.response.var <- function(object, response){
	UseMethod("modify.response.var")
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.response.var Default S3 method.
#'	@method modify.response.var default
#-------------------------------------------------------------------------------
#	defaultはresponseをそのまま返す。
#-------------------------------------------------------------------------------
modify.response.var.default <- function(object, response){
	return(response)
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.response.var
#'	Method for \code{\link[tree]{tree}} class of \emph{tree} package.
#'	@method modify.response.var tree
#-------------------------------------------------------------------------------
#	tree, rpart, randomForest, svm は応答変数がTRUE/FALSE、0/1だけだったら、
#	因子型に変換して返す。
#-------------------------------------------------------------------------------
modify.response.var.tree <- function(object, response){
	return(response.var.to.factor(response))
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.response.var
#'	Method for \code{\link[rpart]{rpart}} class in \emph{rpart} package.
#'	@method modify.response.var rpart
#-------------------------------------------------------------------------------
modify.response.var.rpart <- function(object, response){
	return(response.var.to.factor(response))
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.response.var
#'	Method for \code{\link[randomForest]{randomForest}} class in
#'	\emph{randomForest} package.
#'	@method modify.response.var randomForest
#-------------------------------------------------------------------------------
modify.response.var.randomForest <- function(object, response){
	return(response.var.to.factor(response))
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.response.var
#'	Method for BinaryTree class made by \code{\link[party]{ctree}} function in
#'	\emph{party} package.
#'	@method modify.response.var BinaryTree
#-------------------------------------------------------------------------------
modify.response.var.BinaryTree <- function(object, response){
	return(response.var.to.factor(response))
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.response.var
#'	Method for RandomForest class made by \code{\link[party]{cforest}} function 
#'	in \emph{party} package.
#'	@method modify.response.var RandomForest
#-------------------------------------------------------------------------------
modify.response.var.RandomForest <- function(object, response){
	return(response.var.to.factor(response))
}

#-------------------------------------------------------------------------------
#'	@describeIn modify.response.var
#'	Method for \code{\link[e1071]{svm}} class in \emph{svm} package.
#'	@method modify.response.var svm
#-------------------------------------------------------------------------------
modify.response.var.svm <- function(object, response){
	return(response.var.to.factor(response))
}

#-------------------------------------------------------------------------------
#'	Make response variable to factor.
#'
#'	Internal function making response variable to factor.
#'	@inheritParams modify.response.var
#'	@return A vector containing modified/unmodified response variable
#-------------------------------------------------------------------------------
#	応答変数変換処理を実行する内部関数。
#-------------------------------------------------------------------------------
response.var.to.factor <- function(response){
	if (is(response, "factor")){
		return(response)
	}
	unique.values <- sort(unique(response))
	if (
		identical(unique.values, c(FALSE, TRUE))
		| identical(unique.values, c(0, 1))
		| identical(unique.values, c(0L, 1L))
	){
		response <- as.factor(response)
	}
	return(response)
}

