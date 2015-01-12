#===============================================================================
#	���f���̉����ϐ������q�^�������Ƃ��Apredict�̌��ʂ���z���Ƃ��Ĉ������q��
#	�m�������o���B
#
#	Args:
#		response: �����ϐ��̐��f�[�^�B
#		prediction: predict�̌���
#		positive.label:
#			�z���Ƃ��Ĉ����N���X��\��������B�w�肳��Ȃ������ꍇ��
#			(TRUE, FALSE), (1, 0), (+, -), (+, 0)�̃Z�b�g�̍�����z���Ƃ��Ĉ����A
#			�������I�ɗz���Ƃ��ăf�[�^�̎擾�����݂�B����ł��z��������ł��Ȃ�
#			�ꍇ�A�N���X�̂P�Ԗڂ�z���Ƃ��Ĉ����B
#			���q���R�N���X�ȏゾ�����ꍇ�A�P��ڂ̊m����p����B
#
#	Value:
#		response�����q�^�łȂ��ꍇ�A���̂܂�prediction��Ԃ��B
#		response�����q�^�̏ꍇ�A�z���̏�̕��@�Ŕ��肵���z���̊m����Ԃ��B
#===============================================================================

get.positive.prob <- function(response, prediction, positive.label = NULL){
	# ���q�^�ȊO�͂��̂܂ܒl��Ԃ��B
	if (!is.factor(response)){
		return(prediction)
	}
	# �ǂ̃��x�����z�����w�肳��Ă�����A���̃��x���̊m����Ԃ��B
	if (!is.null(positive.label)){
		return(prediction[, positive.label])
	}
	# �R�N���X�ȏ�̈��q�^�͂P�Ԗڂ̈��q���z�����Ɖ��肵�Ēl��Ԃ��B
	if (nlevels(response) > 2){
		warning("Number of classes of response variable > 2 and 'positive.label' was not specified! \nMetrics were calculated by assuming the first level of the response variable is the positive case.")
		return(prediction[, 1])
	}
	# TRUE, 1, + �����x���Ɋ܂܂�Ă�����A������z���Ɖ��肵�Ēl��Ԃ��B
	classes <- levels(response)
	for (i in list(c("TRUE", "FALSE"), c("1", "0"), c("+", "-"), c("+", "0"))){
		if (identical(classes, i) | identical(classes, rev(i))){
			return(prediction[, i[1]])
		}
	}
	# ����ł��w�W������ł��Ȃ��Ƃ��ɂ͂P�Ԗڂ��z�����Ɖ��肵�Ēl��Ԃ��B
	warning("'positive.label' was not specified and could not determine positive class label. \nMetrics were calculated by assuming the first level of the response variable is the positive case.")
	return(prediction[, 1])
}
