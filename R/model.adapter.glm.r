#-------------------------------------------------------------------------------
#	glm�֐��p��.model.adapter�N���X�̃W�F�l���[�^�[�N���X�B
#-------------------------------------------------------------------------------
.model.adapter.glm <- setRefClass(
	"model.adapter.glm", contains = "model.adapter",
	methods = list(
		get.model.type = function(cv.dummy, args.model, data){
			return(get.model.type.from.family(args.model))
		}
	)
)

