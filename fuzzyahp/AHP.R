# AHP (Analytic Hierachy Process) による評価を行う
AHP <- function(	x,					# 評価基準の重要度（下三角行列をベクトルで用意）
			y,					# 代替案の評価（各行列の下三角行列を列とする行列で用意）
			labels.x=NULL,				# 評価基準のラベル
			labels.y=NULL)				# 代替案のラベル
{
	items <- function(n)					# 下三角行列の要素数から行列サイズを求める
	{
		retval <- (1+sqrt(1+8*n))/2
		return(if (retval!=floor(retval)) Inf else retval)
	}
	make.matrix <- function(x)				# 正方行列から重みベクトルを求める
	{
		n <- items(length(x))				# 行列のサイズ
		mat <- diag(n)					# 三角行列を表すベクトルから行列を生成
		mat[lower.tri(mat, diag=FALSE)] <- x
		mat <- t(mat)+mat
		mat[upper.tri(mat)] <- 1/mat[upper.tri(mat)]
		diag(mat) <- 1
		result <- eigen(mat)				# 固有値・固有ベクトルを求める
		val <- as.numeric(result$values[1])
		vec <- as.numeric(result$vectors[,1])
		weight <- vec/sum(vec)				# 固有ベクトルを和が 1 になるように標準化したものが重み
		ci <- (val-n)/(n-1)
		cr <- ci/c(0,0,0.58,0.9,1.12,1.24,1.32,1.41,1.45,1.49,1.51,1.53)[n]
		if (ci > 0.1 || cr > 0.1) {
			cat("\nC.I.=", ci, ",  C.R.=", cr, "\n", sep="")
			print(mat)
			W <- outer(weight, weight, "/")
			print(W)
			print(mat-W)
		}
		return(list(lambda=val, vec=vec, weight=weight, ci=ci, cr=cr))
	}
	if (is.null(labels.x)) {				# ラベルが与えられていないときは A, B, ...
		labels.x <- LETTERS[1:items(length(x))]
	}
	ans.x <- make.matrix(x)
	weight.x <- ans.x$weight				# 評価基準の重要度
	names(weight.x) <- labels.x
	nitems.y <- items(nrow(y))
	if (is.null(labels.y)) {				# ラベルが与えられていないときは a, b, ...
		labels.y <- letters[1:nitems.y]
	}
	ans.y <- matrix(unlist(apply(y, 2, make.matrix)), 3+2*nitems.y)
	weight.y <- ans.y[(2+nitems.y):(1+2*nitems.y),]		# 代替案の評価
	rownames(weight.y) <- labels.y
	colnames(weight.y) <- labels.x
	score <- rowSums(t(weight.x*t(weight.y)))		# スコア
	return(structure(list(weight.x=weight.x, weight.y=weight.y, score=score, sorted.score=sort(score)), class="AHP"))
}
# print メソッド
print.AHP <- function(	obj,					# AHP の返すオブジェクト
			digits=5)				# 結果の表示桁数
{
	cat("\n評価基準の重み\n\n")
	print(round(obj$weight.x, digits=digits))
	cat("\n代替案の評価結果\n\n")
	print(round(obj$weight.y, digits=digits))
	cat("\nスコア\n\n")
	print(round(obj$score, digits=digits))
	cat("\nソートされたスコア\n\n")
	print(round(obj$sorted.score, digits=digits))
}
# plot メソッド
plot.AHP <- function(	obj,					# AHP の返すオブジェクト
			xlab="Score",				# 結果グラフの横軸名
			main="AHP (Analytic Hierachy Process)",	# 結果グラフの表題
			file="")				# 結果グラフをファイル出力するときにファイル名
{
	if (file != "") pdf(file, width=540/72, height=160/72, onefile=FALSE)
	score <- obj$score
	plot(score, rep(0, length(score)), pch=19, xlab=xlab, main=main, xaxt="n",
		xlim=range(pretty(score)), ylab="", yaxt="n", ylim=c(0,0.2),
		bty="n", xpd=TRUE)
	text(score, 0.0, names(score), pos=3)
	axis(1, pos=0)
	if (file != "") dev.off()
}
