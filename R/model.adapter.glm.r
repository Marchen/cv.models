#-------------------------------------------------------------------------------
#	glm関数用の.model.adapterクラスのジェネレータークラス。
#-------------------------------------------------------------------------------
.model.adapter.glm <- setRefClass(
	"model.adapter.glm", contains = "model.adapter"
)

#-------------------------------------------------------------------------------
#'	@describeIn detect.model.type
#'	method for \code{\link[stats]{glm}} in \emph{stats} package.
#'	@method detect.model.type glm
#-------------------------------------------------------------------------------
.model.adapter.glm$methods(
	get.model.type = function(){
		return(get.model.type.from.family())
	}
)
