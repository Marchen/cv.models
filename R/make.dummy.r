
#-------------------------------------------------------------------------------
#	�֐��������ɂ��āA���̊֐����Ԃ��I�u�W�F�N�g�̋U����Ԃ��֐��B
#
#	Args:
#		function.name: �֐�����\��������B
#-------------------------------------------------------------------------------
make.dummy <- function(function.name){
	object <- list()
	class(object) <- get.class.name(function.name)
	return(object)
}
