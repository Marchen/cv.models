#-------------------------------------------------------------------------------
#'	model.adapter class for gamm
#'
#'	This reference class contains methods for \code{\link[mgcv]{gamm}} in 
#'	\emph{mgcv} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	gamm関数用の.model.adapterクラスのジェネレータークラス。
#-------------------------------------------------------------------------------
.model.adapter.gamm <- setRefClass(
	"model.adapter.gamm", contains = "model.adapter"
)

#-------------------------------------------------------------------------------
#	モデルの種類を返す。
#-------------------------------------------------------------------------------
.model.adapter.gamm$methods(
	get.model.type = function(){
		"
		return a character vector specifying model type 
		(regression or classification).
		"
		return(get.model.type.from.family())
	}
)

#-------------------------------------------------------------------------------
#	formulaの.を展開する。
#-------------------------------------------------------------------------------
.model.adapter.gamm$methods(
	expand.dot = function(){
		"
		expand dot ('.') in model formula used for modeling and store it in
		settings$args.model.
		"
		.self$callSuper(specials = c("s", "te", "ti", "t2"))
	}
)


