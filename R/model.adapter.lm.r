#-------------------------------------------------------------------------------
#	lm�֐��p��.model.adapter�N���X�̃W�F�l���[�^�[�N���X�B
#-------------------------------------------------------------------------------
.model.adapter.lm <- setRefClass(
	"model.adapter.lm", contains = "model.adapter"
)

#-------------------------------------------------------------------------------
#'	@describeIn detect.model.type
#'	method for \code{\link[stats]{lm}} in \emph{stats} package.
#'	@method detect.model.type lm
#-------------------------------------------------------------------------------
.model.adapter.lm$methods(
	get.model.type = function(){
		return("regression")
	}
)


