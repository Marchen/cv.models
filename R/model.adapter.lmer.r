#-------------------------------------------------------------------------------
#'	model.adapter class for lmer
#'
#'	This reference class contains methods for \code{\link[lme4]{lmer}} in 
#'	\emph{lme4} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	lmer�֐��p��.model.adapter�N���X�̃W�F�l���[�^�[�N���X�B
#-------------------------------------------------------------------------------
.model.adapter.lmer <- setRefClass(
	"model.adapter.lmer", contains = "model.adapter"
)

#-------------------------------------------------------------------------------
#	���f���̎�ނ�Ԃ��B
#-------------------------------------------------------------------------------
.model.adapter.lmer$methods(
	get.model.type = function(){
		"
		return a character vector specifying model type 
		(regression or classification).
		"
		return("regression")
	}
)

