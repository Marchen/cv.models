
#-------------------------------------------------------------------------------
#	�֐��������ɂ��āA���̊֐����Ԃ��I�u�W�F�N�g�̋U����Ԃ��֐��B
#
#	Args:
#		function.name: �֐�����\��������B
#-------------------------------------------------------------------------------
make.dummy <- function(function.name){
	object <- list()
	class(object) <- c(get.class.name(function.name), "cv.models.dummy")
	return(object)
}
