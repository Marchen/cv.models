#-------------------------------------------------------------------------------
#	lmer�֐��p��.model.adapter�N���X�̃W�F�l���[�^�[�N���X�B
#-------------------------------------------------------------------------------
.model.adapter.lmer <- setRefClass(
	"model.adapter.lmer", contains = "model.adapter",
	methods = list(
		get.model.type <- function(cv.dummy, args.model, data){
			return(MODEL_TYPE_REGRESSION)
		}
	)
)

