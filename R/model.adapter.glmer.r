#-------------------------------------------------------------------------------
#	glmer�֐��p��.model.adapter�N���X�̃W�F�l���[�^�[�N���X�B
#-------------------------------------------------------------------------------
.model.adapter.glmer <- setRefClass(
	"model.adapter.glmer", contains = "model.adapter"
)

#-------------------------------------------------------------------------------
#'	@describeIn detect.model.type
#'	method for \code{\link[lme4]{glmer}} in \emph{lme4} package.
#'	@method detect.model.type glmerMod
#-------------------------------------------------------------------------------
.model.adapter.glmer$methods(
	get.model.type = function(){
		return(get.model.type.from.family())
	}
)


