#-------------------------------------------------------------------------------
#'	model.adapter class for glmer
#'
#'	This reference class contains methods for \code{\link[lme4]{glmer}} in 
#'	\emph{lme4} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	glmer�֐��p��.model.adapter�N���X�̃W�F�l���[�^�[�N���X�B
#-------------------------------------------------------------------------------
.model.adapter.glmer <- setRefClass(
	"model.adapter.glmer", contains = "model.adapter"
)

#-------------------------------------------------------------------------------
#	���f���̎�ނ�Ԃ��B
#-------------------------------------------------------------------------------
.model.adapter.glmer$methods(
	get.model.type = function(){
		"
		return a character vector specifying model type 
		(regression or classification).
		"
		return(get.model.type.from.family())
	}
)


