#-------------------------------------------------------------------------------
#	���f���̍\�z��predict�Ɏg����������ꂽargs���X�g�̒�����A���f���̐��\��
#	�e��������p�����[�^�[�����o�����̊֐��B
#	���f���̍\�z����gbm��shrinkage�Ƃ�interaction.depth�Apredict����gbm��n.trees
#	�݂����Ȃ̂ɑΉ����邽�߂̊֐��B
#
#	Args:
#		model:
#			�N���X�𔻕ʂ��邽�߂̃��f���I�u�W�F�N�g�B�v�Z�ɂ͎g���Ȃ��̂ŁA
#			���̂͋��OK�B
#		args.predict: predict�ɓn���������̓��������X�g�B
#		type:
#			"model": ���f���\�z�ɓn�����������Ɖ��肵�ăp�����[�^�[�����o���B
#			"predict": predict�ɓn�����������Ɖ��肵�ăp�����[�^�[�����o���B
#
#	Value:
#		���p�����[�^�[�̓��������X�g�B
#-------------------------------------------------------------------------------
get.tunable.args <- function(model, args, type){
	UseMethod("get.tunable.args")
}
# default�͉������Ȃ��B
get.tunable.args.default <- function(model, args, type){
	return(NULL)
}
# gbm
get.tunable.args.gbm <- function(model, args, type){
	arg.names <- switch(
		type,
		model = c(
			"n.minobsinnode", "interaction.depth", "bag.fraction", "shrinkage"
		),
		predict = "n.trees"
	)
	return(get.args(args, arg.names))
}
# randomForest
get.tunable.args.randomForest <- function(model, args, type){
	if (type == "predict"){
		return(NULL)
	} else {
		return(get.args(args, c("mtry","sampsize", "nodesize", "maxnodes")))
	}
}