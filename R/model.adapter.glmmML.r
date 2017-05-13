#-------------------------------------------------------------------------------
#'	model.adapter class for glmmML
#'
#'	This reference class contains methods for \code{\link[glmmML]{glmmML}} in 
#'	\emph{glmmML} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	glm関数用の.model.adapterクラスのジェネレータークラス。
#-------------------------------------------------------------------------------
.model.adapter.glmmML <- setRefClass(
	"model.adapter.glmmML", contains = "model.adapter"
)

#-------------------------------------------------------------------------------
#	モデルの種類を返す。
#-------------------------------------------------------------------------------
.model.adapter.glmmML$methods(
	get.model.type = function(){
		"
		return a character vector specifying model type 
		(regression or classification).
		"
		return(get.model.type.from.family())
	}
)
