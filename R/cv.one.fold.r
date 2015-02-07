
#-------------------------------------------------------------------------------
#	�P��fold�ŃN���X�o���f�[�V��������֐��B
#
#	Args:
#		model.function: ���f�������֐��Bglm�Ƃ��Agbm�Ƃ��B
#		args.model: ���f���\�z�Ɏg��������B
#		args.predict:
#			predict�֐��ɓn���������Bgbm��n.trees�݂����ɗ\�����\�ɉe������
#			�p�����[�^�[��c()�֐��ŕ����^����Ƃ��ꂼ��̒l���g���āA�\���l��
#			�v�Z����B�������A���̃N���X�p��get.tunable.args�֐�������đΉ�
#			����K�v������B
#		data: ���f���\�z�Ɏg���f�[�^
#		cv.group: �N���X�o���f�[�V�����̃O���[�v�B
#		cv.index: �N���X�o���f�[�V�����Ńe�X�g�f�[�^�ɂ���O���[�v�ԍ��B
#		seed: �����̎�q�B
#		positive.class: �����ϐ������q�^�̎��A�z���Ƃ��Ĉ����N���X��\��������B
#		
#	Value:
#		�e�X�g�f�[�^���g�����\���l�Ɖ����ϐ��̎����l���������s��B
#		�����l�̗񖼂�y�ɂȂ�B�\���l�̗񖼂̓��f���ˑ��B
#-------------------------------------------------------------------------------
cv.one.fold <- function(
	model.function, args.model, args.predict, data, cv.group, cv.index, seed,
	positive.class = NULL
){
	# ���f���\�z
	if (!is.null(seed)) set.seed(seed)
	data.test <- data[cv.group == cv.index, ]
	data.train <- data[cv.group != cv.index, ]
	args.model$data <- data.train
	model <- do.call(model.function, args.model)
	# �w�W���������v�Z�ł���悤�ɁApredict�̈������C��
	args.predict$object <- model
	args.predict$newdata <- data.test
	# �\���l���v�Z
	predictions <- do.call(predict, args.predict)
	predictions <- format.prediction(model, predictions)
	# �\���l�̒�����z���̊m�������o���B
	response <- data.test[[get.response.name(model)]]
	predictions <- get.positive.prob(response, predictions, positive.class)
	# ���ʂ𐮌`
	result <- cbind(y = response, as.data.frame(predictions))
	rownames(result) <- rownames(data.test)
	return(result)
}
