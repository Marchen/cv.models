#-------------------------------------------------------------------------------
#	���f���̐��\�]���w�W�ɂ��̂Ƃ��Ɏg��ꂽ�����\�ȃp�����[�^�[�̒l��ǉ�����
#	�⏕�֐��B
#
#	Args:
#		cv.dummy: �U�̐��茋�ʂ�cv.dummy�I�u�W�F�N�g�B
#		metrics: ���f���̐��\�]���w�W���������s��B
#		args: ���f���쐬��������predict�ɓn�������������������X�g�B
#		type:
#			"model":
#				args�����f���\�z�ɓn�����������Ɖ��肵�ăp�����[�^�[��ǉ��B
#			"predict":
#				args��predict�ɓn�����������Ɖ��肵�ăp�����[�^�[��ǉ��B
#-------------------------------------------------------------------------------
merge.tunable.args <- function(cv.dummy, metrics, args, type){
	# �`���[�j���O�������p�����[�^�[���擾����
	tunable.args <- get.tunable.args(cv.dummy, args, type)
	if (is.null(tunable.args)){
		# �Ȃ���΂��̂܂�metrics��Ԃ�
		return(metrics)
	}
	# ��������f�[�^��p�ӁB�s�����c���Ă���ƌx�����o��̂ŁA�s���͏����B
	grid <- do.call(expand.grid, tunable.args)
	grid <- split(grid, 1:nrow(grid))
	grid <- lapply(grid , "rownames<-", NULL)
	map.args <- c(list(metrics), list(grid), f = cbind)
	return(do.call(Map, map.args))
}
