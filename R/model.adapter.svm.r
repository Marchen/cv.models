#-------------------------------------------------------------------------------
#	svm�֐��p��.model.adapter�N���X�̃W�F�l���[�^�[�N���X�B
#-------------------------------------------------------------------------------
.model.adapter.svm <- setRefClass(
	"model.adapter.svm", contains = "model.adapter",
	methods = list(
		modify.args.model = function(){
			args.model <- settings$args.model
			args.model$probability <- TRUE
			settings$args.model.src <<- settings$args.model
			settings$args.model <<- args.model
		}
	)
)

