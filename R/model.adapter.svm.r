#-------------------------------------------------------------------------------
#'	model.adapter class for svm
#'
#'	This reference class contains methods for \code{\link[e1071]{svm}} in 
#'	\emph{e1071} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	svm�֐��p��.model.adapter�N���X�̃W�F�l���[�^�[�N���X�B
#-------------------------------------------------------------------------------
.model.adapter.svm <- setRefClass(
	"model.adapter.svm", contains = "model.adapter"
)

#-------------------------------------------------------------------------------
#	args.model��probability��TRUE�ɂ���B
#-------------------------------------------------------------------------------
.model.adapter.svm$methods(
	modify.args.model = function(){
		"
		This function set \\emph{probability} in \\emph{args.model} to TRUE.
		"
		args.model <- settings$args.model
		args.model$probability <- TRUE
		settings$args.model.src <<- settings$args.model
		settings$args.model <<- args.model
	}
)

