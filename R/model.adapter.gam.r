#-------------------------------------------------------------------------------
#'	model.adapter class for gam
#'
#'	This reference class contains methods for \code{\link[mgcv]{gam}} in 
#'	\emph{mgcv} package and \code{\link[gam]{gam}} in \emph{gam} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	gam関数用の.model.adapterクラスのジェネレータークラス。
#-------------------------------------------------------------------------------
.model.adapter.gam <- setRefClass(
	"model.adapter.gam", contains = "model.adapter"
)

#-------------------------------------------------------------------------------
#	モデルの種類を返す。
#-------------------------------------------------------------------------------
.model.adapter.gam$methods(
	get.model.type = function(){
		return(get.model.type.from.family())
	}
)

#-------------------------------------------------------------------------------
#	formulaの.を展開する。
#	mgcv::gamとgam::gamで動作を変える。
#-------------------------------------------------------------------------------
.model.adapter.gam$methods(
	expand.dot = function(){
		# mgcv::gamとgam:gamで特殊文字の種類を変える。
		if (settings$package.name == "mgcv"){
			.self$callSuper(specials = c("s", "te", "ti", "t2"))
		} else {
			require(gam)
			.self$callSuper(specials = gam::gam.slist)
		}
	}
)


