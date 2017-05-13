#-------------------------------------------------------------------------------
#'	model.adapter class for lme
#'
#'	This reference class contains methods for \code{\link[nlme]{lme}} in 
#'	\emph{nlme} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	lme�֐��p��.model.adapter�N���X�̃W�F�l���[�^�[�N���X�B
#-------------------------------------------------------------------------------
.model.adapter.lme <- setRefClass(
	"model.adapter.lme", contains = "model.adapter"
)

#-------------------------------------------------------------------------------
#	���f���̎�ނ�Ԃ��B
#-------------------------------------------------------------------------------
.model.adapter.lme$methods(
	get.model.type = function(){
		"
		return a character vector specifying model type 
		(regression or classification).
		"
		return("regression")
	}
)

#-------------------------------------------------------------------------------
#	���f���\�z�Ɏg����������烂�f����������킷formula���擾����B
#-------------------------------------------------------------------------------
.model.adapter.lme$methods(
	get.formula.lme = function(){
		"
		Retrieving model formula (of fixed effect) from the arguments for the
		modeling function.
		"
		args.model <- settings$args.model
		if (!is.null(args.model$fixed)){
			return(args.model$fixed)
		}
		# �����Ɋ܂܂��formula�̂Ȃ���random���폜���āA�P�Ԗڂɂ�����̂�Ԃ��B
		formulae <- args.model[sapply(args.model, is.formula)]
		formulae$random <- NULL
		return(formulae[[1]])
	}
)

