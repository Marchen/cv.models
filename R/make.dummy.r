
#-------------------------------------------------------------------------------
#	�֐��������ɂ��āA���̊֐����Ԃ��I�u�W�F�N�g�̋U����Ԃ��֐��B
#
#	Args:
#		function.name: �֐�����\��������B
#-------------------------------------------------------------------------------
make.dummy <- function(function.name, package.name){
	object <- list(package = package.name)
	class(object) <- c(get.class.name(function.name), "cv.dummy")
	return(object)
}
