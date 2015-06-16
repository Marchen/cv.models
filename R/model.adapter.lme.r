#-------------------------------------------------------------------------------
#	lme�֐��p��.model.adapter�N���X�̃W�F�l���[�^�[�N���X�B
#-------------------------------------------------------------------------------
.model.adapter.lme <- setRefClass(
	"model.adapter.lme", contains = "model.adapter",
	methods = list(
		get.model.type <- function(cv.dummy, args.model, data){
			return(MODEL_TYPE_REGRESSION)
		}
	)
)

#-------------------------------------------------------------------------------
#'	@describeIn get.formula
#'	Method for \code{\link[nlme]{lme}} function in \emph{nlme} package.
#'	@method get.formula lme
#-------------------------------------------------------------------------------
.model.adapter.lme$methods(
	get.formula.lme = function(){
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

