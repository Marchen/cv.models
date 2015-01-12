#-------------------------------------------------------------------------------
#	���f���쐬�Ɏg���p�����[�^�[�̂Ȃ��ŁA���\�ɉe������p�����[�^�[���x�N�g����
#	�����w�肳�ꂽ�Ƃ��A���̑S�Ă̑g�ݍ��킹������ă��X�g�ŕԂ��⏕�֐��B
#
#	Args:
#		object:
#			���f���I�u�W�F�N�g�B�v�Z�ɂ͎g���Ȃ��̂ŃN���X����������΋�ł�OK�B
#		args.model: ���f���쐬�Ɏg������������������X�g�B
#		type
#			"model": ���f���\�z�ɓn�����������Ɖ��肵�ăp�����[�^�[�����o���B
#			"predict": predict�ɓn�����������Ɖ��肵�ăp�����[�^�[�����o���B
#
#	Value:
#		���f���쐬�Ɏg������������������X�g�ŕ����w�肳�ꂽ�����A���̑g��
#		���킹�̈�Œu��������ꂽ���X�g�����������X�g�B
#		list(args.model, args.model, args.model)�Ƃ����\���̃f�[�^�B
#-------------------------------------------------------------------------------
expand.tunable.args <- function(object, args, type){
	# ���p�����[�^�[�̑g�ݍ��킹���쐬�B
	tunable.args <- get.tunable.args(object, args, type)
	if (is.null(tunable.args)){
		return(list(args))
	}
	grid <- do.call(expand.grid, tunable.args)
	args[names(tunable.args)] <- NULL
	# ���f���쐬�̈��������쐬
	expanded.args <- replicate(nrow(grid), args, simplify = FALSE)
	expanded.args <- Map(c, expanded.args, split(grid, 1:nrow(grid)))
	return(expanded.args)
}