#-------------------------------------------------------------------------------
#'	model.adapter class for gamm
#'
#'	This reference class contains methods for \code{\link[mgcv]{gamm}} in 
#'	\emph{mgcv} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	gamm�֐��p��.model.adapter�N���X�̃W�F�l���[�^�[�N���X�B
#-------------------------------------------------------------------------------
.model.adapter.gamm <- setRefClass(
	"model.adapter.gamm", contains = "model.adapter"
)

#-------------------------------------------------------------------------------
#	���f���̎�ނ�Ԃ��B
#-------------------------------------------------------------------------------
.model.adapter.gamm$methods(
	get.model.type = function(){
		"
		return a character vector specifying model type 
		(regression or classification).
		"
		return(get.model.type.from.family())
	}
)

#-------------------------------------------------------------------------------
#	formula��.��W�J����B
#-------------------------------------------------------------------------------
.model.adapter.gamm$methods(
	expand.dot = function(){
		"
		expand dot ('.') in model formula used for modeling and store it in
		settings$args.model.
		"
		.self$callSuper(specials = c("s", "te", "ti", "t2"))
	}
)


