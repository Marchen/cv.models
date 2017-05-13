#-------------------------------------------------------------------------------
#'	model.adapter class for glm
#'
#'	This reference class contains methods for \code{\link[stats]{glm}} in 
#'	\emph{stats} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	glm関数用の.model.adapterクラスのジェネレータークラス。
#-------------------------------------------------------------------------------
.model.adapter.glm <- setRefClass(
	"model.adapter.glm", contains = "model.adapter"
)

#-------------------------------------------------------------------------------
#	モデルの種類を返す。
#-------------------------------------------------------------------------------
.model.adapter.glm$methods(
	get.model.type = function(){
		"
		return a character vector specifying model type 
		(regression or classification).
		"
		return(get.model.type.from.family())
	}
)
