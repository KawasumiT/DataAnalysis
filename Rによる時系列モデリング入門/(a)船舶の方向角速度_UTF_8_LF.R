## 関数解像
# unicor関数
myUnicorPlot <- function(data_, lag_, main_ = ""){
	unicor01 <- unicor(data_, lag=lag_, plot=FALSE)$acov
	plot(unicor01, type='h', main=main_)
	points(unicor01, type='l')
	points(cbind(seq(0, lag_ + 1, 1), 0), type="l")
}

#
panel.hist <- function(x, ...)
{
	usr <- par("usr"); on.exit(par(usr))
	par(usr = c(usr[1:2], 0, 1.5) )
	h <- hist(x, plot = FALSE)
	breaks <- h$breaks; nB <- length(breaks)
	y <- h$counts; y <- y/max(y)
	rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

## data
library(TSSS)
data(HAKUSAN) # 船舶の航行中の多変量データ
data(Sunspot) # 太陽黒点数データ
data(Temperature) # 東京の日最高気温データ
data(BLSALLFOOD) # アメリカの食品産業の従業者数
data(WHARD) # あるハードウェアの卸売高データ
data(MYE1F) # 地震波データ
data(Nikkei225) # 日経225 平均株価データ
data(Rainfall) # 各月日について東京で２年間に雨が振った回数
data(Haibara) # 地下水位データ

## (a) 船舶の方向角速度
plot(HAKUSAN[,1], main="(a) 船舶の方向角速度")
# 外洋を航行中の船舶の角速度を１秒ごとに記録したもの
# 針路を一定に保針制御があり方向角速度は０前後を変動している

# 定常
# 一見したところ時間的に変化しない一定の確率モデルとみなすことができるもの

# 散布図で表現
par( mfrow=c(4,2), mar=c(2,2.5,3,1) + 0.1)

plot(HAKUSAN[,1], main="(a) 方向角速度")
plot(HAKUSAN[,2], main="(b) 横揺れ")

hist(HAKUSAN[,1], main="(a) 方向角速度")
hist(HAKUSAN[,2], main="(b) 横揺れ", breaks=seq(-6, 12, 1.2))

x <- as.ts(HAKUSAN[,1])
y <- as.ts(HAKUSAN[,2])
plot(x, lag(x, k=2), pch=20, main="(c) lag=2")
plot(y, lag(y, k=2), pch=20, main="(d) lag=2")
plot(x, lag(x, k=4), pch=20, main="(e) lag=4")
plot(y, lag(y, k=4), pch=20, main="(f) lag=4")

# 自己相関関数
par(mfrow=c(2,2), mar=c(2,2.5,3,1) + 0.1)
plot(HAKUSAN[,1], main="(a) 船舶の方向角速度")
plot(HAKUSAN[,2], main="(b) 横揺れ")

myUnicorPlot(HAKUSAN[,1], lag=50, main="(a)の自己相関関数")
myUnicorPlot(HAKUSAN[,2], lag=50, main="(a)の自己相関関数")

# 定常時系列の場合、標本自己相関関数はラグの増加とともに急速に０に減衰している
