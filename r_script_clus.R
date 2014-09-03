# R�o�b�`���T���v���X�N���v�g

# �O��:���x�������f�[�^�̍쐬

# ����:k-means�N���X�^�����O���s��A�d�S�ɋ߂��f�[�^�|�C���g��ԋp
# ����[data]: �N���X�^�����O�Ώۂ̃f�[�^(���t����)
# ����[num]: �N���X�^���A�f�t�H���g�l:3
# �ߒl[cls.rank]: ���X�̃f�[�^�A�N���X�^���A���������f�[�^�t���[��
cls.rank <- function(data, num=3) {
  
  # k-means�N���X�^�����O�̎��s
  # ���ʂ̍Č�����ۂ��߁A����������O�ɐݒ�
  set.seed(5)
  data.k.means <- kmeans(data,num)
  
  # k-means�N���X�^�����O���ʂƃ��x�������f�[�^�̌���
  # �e�f�[�^�̑�����N���X�^�����擾
  cls <- data.k.means$cluster
  # ���f�[�^(���t����)�ƃN���X�^���̌���(�����)
  data.cls <- cbind(data, cls)
  
  # ���f�[�^�̗񐔎擾
  colnum = ncol(data)
  
  # �߂�l�̈�̎��O��`
  # �N���X�^������o�͍s��(�Œ�:5)�~�N���X�^���A1+���f�[�^�����̌�A"����"�J������ǉ�
  res <- data.frame(matrix(0.0, 5*num, 1+colnum))
  colnames(res) <- colnames(data.cls)
  # "����"��̒ǉ�
  res$dist <- 0.0
    
  for (i in 1:num) {
        
    # �e�N���X�^�ɑ�����f�[�^���擾  
    data.cls.i <- data.cls[data.cls$cls == i,]
        
    # �e�N���X�^�̒��S����̋���(���[�N���b�h����)���擾(�e�N���X�^��)
    # ����:�s��ƃx�N�g���̉��Z�́A��������D�悳��邽�߁A�s�����x�]�u���Ă��牉�Z�A���̌�A�ēx�A�]�u
    d.i <- sqrt(apply(t(t(data.cls.i[,1:colnum]) - data.k.means$centers[i,])^2, 1, sum))
    
    # �e�N���X�^����ŏ������̃f�[�^�|�C���g���擾(�Œ�ŏ��5�ʂ܂�)
    rnum = i*5
    res[(rnum-(5-1)):rnum,] <- data.frame(cbind(data.cls.i[order(d.i),], d.i[order(d.i)])[1:5,])
    
  }
  
  return (res)
}