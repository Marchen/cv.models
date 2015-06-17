#-------------------------------------------------------------------------------
#	lm関数用の.model.adapterクラスのジェネレータークラス。
#-------------------------------------------------------------------------------
.model.adapter.lm <- setRefClass(
	"model.adapter.lm", contains = "model.adapter"
)

#-------------------------------------------------------------------------------
#'	@describeIn detect.model.type
#'	method for \code{\link[stats]{lm}} in \emph{stats} package.
#'	@method detect.model.type lm
#-------------------------------------------------------------------------------
.model.adapter.lm$methods(
	get.model.type = function(){
		return("regression")
	}
)


