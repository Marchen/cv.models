#-------------------------------------------------------------------------------
#	args�Ŏw�肳�ꂽ���X�g�̒�����arg.names�Ŏw�肳�ꂽ�f�[�^�����o���⏕�֐��B
#	args�̒��Ƀf�[�^���Ȃ����͖̂��������iNULL��NA�͕Ԃ�Ȃ��j�B
#
#	Args:
#		args: �f�[�^���������X�g�B
#		arg.names: ���o���ϐ����̃x�N�g���B
#
#	Value:
#		�f�[�^�̓��������X�g�B
#-------------------------------------------------------------------------------
get.args <- function(args, arg.names){
	arg.names <- arg.names[arg.names %in% names(args)]
	result <- args[arg.names]
	if (length(result) == 0 ){
		result <- NULL
	}
	return(result)
}