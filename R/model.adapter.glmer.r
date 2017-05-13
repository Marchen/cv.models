#-------------------------------------------------------------------------------
#'	model.adapter class for glmer
#'
#'	This reference class contains methods for \code{\link[lme4]{glmer}} in 
#'	\emph{lme4} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	glmer関数用の.model.adapterクラスのジェネレータークラス。
#-------------------------------------------------------------------------------
.model.adapter.glmer <- setRefClass(
	"model.adapter.glmer", contains = "model.adapter"
)

#-------------------------------------------------------------------------------
#	モデルの種類を返す。
#-------------------------------------------------------------------------------
.model.adapter.glmer$methods(
	get.model.type = function(){
		"
		return a character vector specifying model type 
		(regression or classification).
		"
		return(get.model.type.from.family())
	}
)


