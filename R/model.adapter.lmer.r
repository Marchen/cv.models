#-------------------------------------------------------------------------------
#	lmer�֐��p��.model.adapter�N���X�̃W�F�l���[�^�[�N���X�B
#-------------------------------------------------------------------------------
.model.adapter.lmer <- setRefClass(
	"model.adapter.lmer", contains = "model.adapter"
)

.model.adapter.lmer$methods(
	get.model.type = function(){
		return("regression")
	}
)

