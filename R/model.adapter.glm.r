#-------------------------------------------------------------------------------
#'	model.adapter class for glm
#'
#'	This reference class contains methods for \code{\link[stats]{glm}} in 
#'	\emph{stats} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	glm�֐��p��.model.adapter�N���X�̃W�F�l���[�^�[�N���X�B
#-------------------------------------------------------------------------------
.model.adapter.glm <- setRefClass(
	"model.adapter.glm", contains = "model.adapter"
)

#-------------------------------------------------------------------------------
#	���f���̎�ނ�Ԃ��B
#-------------------------------------------------------------------------------
.model.adapter.glm$methods(
	get.model.type = function(){
		"
		return a character vector specifying model type 
		(regression or classification).
		"
		return(get.model.type.from.family())
	}
)
