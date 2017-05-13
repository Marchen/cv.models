#-------------------------------------------------------------------------------
#'	model.adapter class for lm
#'
#'	This reference class contains methods for \code{\link[stats]{lm}} in 
#'	\emph{stats} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	lm関数用の.model.adapterクラスのジェネレータークラス。
#-------------------------------------------------------------------------------
.model.adapter.lm <- setRefClass(
	"model.adapter.lm", contains = "model.adapter"
)

#-------------------------------------------------------------------------------
#	モデルの種類を返す。
#-------------------------------------------------------------------------------
.model.adapter.lm$methods(
	get.model.type = function(){
		"
		return a character vector specifying model type 
		(regression or classification).
		"
		return("regression")
	}
)


