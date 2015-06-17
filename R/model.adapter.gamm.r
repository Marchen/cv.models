#-------------------------------------------------------------------------------
#	gamm関数用の.model.adapterクラスのジェネレータークラス。
#-------------------------------------------------------------------------------
.model.adapter.gamm <- setRefClass(
	"model.adapter.gamm", contains = "model.adapter"
)

#-------------------------------------------------------------------------------
#'	@describeIn detect.model.type
#'	method for \code{\link[mgcv]{gamm}} in \emph{mgcv} package.
#'	@method detect.model.type gamm
#-------------------------------------------------------------------------------
.model.adapter.gamm$methods(
	get.model.type = function(){
		return(get.model.type.from.family())
	}
)

#-------------------------------------------------------------------------------
#'	@describeIn expand.dot
#'	Method for \code{\link[mgcv]{gamm}} function in \emph{mgcv} package.
#'	@method expand.dot gamm
#-------------------------------------------------------------------------------
.model.adapter.gamm$methods(
	expand.dot = function(){
		.self$callSuper(specials = c("s", "te", "ti", "t2"))
	}
)


