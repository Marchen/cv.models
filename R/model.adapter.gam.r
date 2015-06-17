#-------------------------------------------------------------------------------
#	gam�֐��p��.model.adapter�N���X�̃W�F�l���[�^�[�N���X�B
#-------------------------------------------------------------------------------
.model.adapter.gam <- setRefClass(
	"model.adapter.gam", contains = "model.adapter"
)

#-------------------------------------------------------------------------------
#'	@describeIn detect.model.type
#'	method for \code{\link[mgcv]{gam}} in \emph{mgcv} package and 
#'	\code{\link[gam]{gam}} in \emph{gam} package.
#'	@method detect.model.type gam
#-------------------------------------------------------------------------------
.model.adapter.gam$methods(
	get.model.type = function(){
		return(get.model.type.from.family())
	}
)

#-------------------------------------------------------------------------------
#'	@describeIn expand.dot
#'	Method for \code{\link[gam]{gam}} function in \emph{gam} package and 
#'	\code{\link[mgcv]{gam}} function \emph{mgcv} package.
#'	@method expand.dot gam
#-------------------------------------------------------------------------------
.model.adapter.gam$methods(
	expand.dot = function(){
		# mgcv::gam��gam:gam�œ��ꕶ���̎�ނ�ς���B
		if (settings$package.name == "mgcv"){
			.self$callSuper(specials = c("s", "te", "ti", "t2"))
		} else {
			require(gam)
			.self$callSuper(specials = gam::gam.slist)
		}
	}
)


