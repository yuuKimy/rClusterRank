# Rバッチ化サンプルスクリプト

# 前提:ラベル無しデータの作成

# 処理:k-meansクラスタリング実行後、重心に近いデータポイントを返却
# 引数[data]: クラスタリング対象のデータ(教師無し)
# 引数[num]: クラスタ数、デフォルト値:3
# 戻値[cls.rank]: 元々のデータ、クラスタ情報、距離を持つデータフレーム
cls.rank <- function(data, num=3) {
  
  # k-meansクラスタリングの実行
  # 結果の再現性を保つため、乱数種を事前に設定
  set.seed(5)
  data.k.means <- kmeans(data,num)
  
  # k-meansクラスタリング結果とラベル無しデータの結合
  # 各データの属するクラスタ情報を取得
  cls <- data.k.means$cluster
  # 元データ(教師無し)とクラスタ情報の結合(列方向)
  data.cls <- cbind(data, cls)
  
  # 元データの列数取得
  colnum = ncol(data)
  
  # 戻り値領域の事前定義
  # クラスタ当たり出力行数(固定:5)×クラスタ数、1+元データ→その後、"距離"カラムを追加
  res <- data.frame(matrix(0.0, 5*num, 1+colnum))
  colnames(res) <- colnames(data.cls)
  # "距離"列の追加
  res$dist <- 0.0
    
  for (i in 1:num) {
        
    # 各クラスタに属するデータを取得  
    data.cls.i <- data.cls[data.cls$cls == i,]
        
    # 各クラスタの中心からの距離(ユークリッド距離)を取得(各クラスタ毎)
    # 注意:行列とベクトルの演算は、列方向が優先されるため、行列を一度転置してから演算、その後、再度、転置
    d.i <- sqrt(apply(t(t(data.cls.i[,1:colnum]) - data.k.means$centers[i,])^2, 1, sum))
    
    # 各クラスタから最小距離のデータポイントを取得(固定で上位5位まで)
    rnum = i*5
    res[(rnum-(5-1)):rnum,] <- data.frame(cbind(data.cls.i[order(d.i),], d.i[order(d.i)])[1:5,])
    
  }
  
  return (res)
}
