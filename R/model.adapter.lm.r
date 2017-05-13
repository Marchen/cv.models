#-------------------------------------------------------------------------------
#'	model.adapter class for lm
#'
#'	This reference class contains methods for \code{\link[stats]{lm}} in 
#'	\emph{stats} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	lm�֐��p��.model.adapter�N���X�̃W�F�l���[�^�[�N���X�B
#-------------------------------------------------------------------------------
.model.adapter.lm <- setRefClass(
	"model.adapter.lm", contains = "model.adapter"
)

#-------------------------------------------------------------------------------
#	���f���̎�ނ�Ԃ��B
#-------------------------------------------------------------------------------
.model.adapter.lm$methods(
	get.model.type = function(){
		"
		return a character vector specifying model type 
		(regression or classification).
		"
		return("regression")
	}
)


