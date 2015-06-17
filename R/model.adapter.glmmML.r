#-------------------------------------------------------------------------------
#	glm関数用の.model.adapterクラスのジェネレータークラス。
#-------------------------------------------------------------------------------
.model.adapter.glmmML <- setRefClass(
	"model.adapter.glmmML", contains = "model.adapter"
)

#-------------------------------------------------------------------------------
#'	@describeIn detect.model.type
#'	method for \code{\link[glmmML]{glmmML}} in \emph{glmmML} package.
#'	@method detect.model.type glmmML
#-------------------------------------------------------------------------------
.model.adapter.glmmML$methods(
	get.model.type = function(){
		return(get.model.type.from.family())
	}
)
