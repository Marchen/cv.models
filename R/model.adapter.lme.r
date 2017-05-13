#-------------------------------------------------------------------------------
#'	model.adapter class for lme
#'
#'	This reference class contains methods for \code{\link[nlme]{lme}} in 
#'	\emph{nlme} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	lme関数用の.model.adapterクラスのジェネレータークラス。
#-------------------------------------------------------------------------------
.model.adapter.lme <- setRefClass(
	"model.adapter.lme", contains = "model.adapter"
)

#-------------------------------------------------------------------------------
#	モデルの種類を返す。
#-------------------------------------------------------------------------------
.model.adapter.lme$methods(
	get.model.type = function(){
		"
		return a character vector specifying model type 
		(regression or classification).
		"
		return("regression")
	}
)

#-------------------------------------------------------------------------------
#	モデル構築に使われる引数からモデル式をあらわすformulaを取得する。
#-------------------------------------------------------------------------------
.model.adapter.lme$methods(
	get.formula.lme = function(){
		"
		Retrieving model formula (of fixed effect) from the arguments for the
		modeling function.
		"
		args.model <- settings$args.model
		if (!is.null(args.model$fixed)){
			return(args.model$fixed)
		}
		# 引数に含まれるformulaのなかでrandomを削除して、１番目にあるものを返す。
		formulae <- args.model[sapply(args.model, is.formula)]
		formulae$random <- NULL
		return(formulae[[1]])
	}
)

