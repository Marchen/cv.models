#-------------------------------------------------------------------------------
#'	model.adapter class for gam
#'
#'	This reference class contains methods for \code{\link[mgcv]{gam}} in 
#'	\emph{mgcv} package and \code{\link[gam]{gam}} in \emph{gam} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	gam�֐��p��.model.adapter�N���X�̃W�F�l���[�^�[�N���X�B
#-------------------------------------------------------------------------------
.model.adapter.gam <- setRefClass(
	"model.adapter.gam", contains = "model.adapter"
)

#-------------------------------------------------------------------------------
#	���f���̎�ނ�Ԃ��B
#-------------------------------------------------------------------------------
.model.adapter.gam$methods(
	get.model.type = function(){
		return(get.model.type.from.family())
	}
)

#-------------------------------------------------------------------------------
#	formula��.��W�J����B
#	mgcv::gam��gam::gam�œ����ς���B
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


