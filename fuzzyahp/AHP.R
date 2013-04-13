# AHP (Analytic Hierachy Process) による删擦を乖う
AHP <- function(	x,					# 删擦答洁の脚妥刨∈布话逞乖误をベクトルで脱罢∷
			y,					# 洛仑捌の删擦∈称乖误の布话逞乖误を误とする乖误で脱罢∷
			labels.x=NULL,				# 删擦答洁のラベル
			labels.y=NULL)				# 洛仑捌のラベル
{
	items <- function(n)					# 布话逞乖误の妥燎眶から乖误サイズを滇める
	{
		retval <- (1+sqrt(1+8*n))/2
		return(if (retval!=floor(retval)) Inf else retval)
	}
	make.matrix <- function(x)				# 赖数乖误から脚みベクトルを滇める
	{
		n <- items(length(x))				# 乖误のサイズ
		mat <- diag(n)					# 话逞乖误を山すベクトルから乖误を栏喇
		mat[lower.tri(mat, diag=FALSE)] <- x
		mat <- t(mat)+mat
		mat[upper.tri(mat)] <- 1/mat[upper.tri(mat)]
		diag(mat) <- 1
		result <- eigen(mat)				# 盖铜猛ˇ盖铜ベクトルを滇める
		val <- as.numeric(result$values[1])
		vec <- as.numeric(result$vectors[,1])
		weight <- vec/sum(vec)				# 盖铜ベクトルを下が 1 になるように筛洁步したものが脚み
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
	if (is.null(labels.x)) {				# ラベルが涂えられていないときは A, B, ...
		labels.x <- LETTERS[1:items(length(x))]
	}
	ans.x <- make.matrix(x)
	weight.x <- ans.x$weight				# 删擦答洁の脚妥刨
	names(weight.x) <- labels.x
	nitems.y <- items(nrow(y))
	if (is.null(labels.y)) {				# ラベルが涂えられていないときは a, b, ...
		labels.y <- letters[1:nitems.y]
	}
	ans.y <- matrix(unlist(apply(y, 2, make.matrix)), 3+2*nitems.y)
	weight.y <- ans.y[(2+nitems.y):(1+2*nitems.y),]		# 洛仑捌の删擦
	rownames(weight.y) <- labels.y
	colnames(weight.y) <- labels.x
	score <- rowSums(t(weight.x*t(weight.y)))		# スコア
	return(structure(list(weight.x=weight.x, weight.y=weight.y, score=score, sorted.score=sort(score)), class="AHP"))
}
# print メソッド
print.AHP <- function(	obj,					# AHP の手すオブジェクト
			digits=5)				# 冯蔡の山绩峰眶
{
	cat("\n删擦答洁の脚み\n\n")
	print(round(obj$weight.x, digits=digits))
	cat("\n洛仑捌の删擦冯蔡\n\n")
	print(round(obj$weight.y, digits=digits))
	cat("\nスコア\n\n")
	print(round(obj$score, digits=digits))
	cat("\nソ〖トされたスコア\n\n")
	print(round(obj$sorted.score, digits=digits))
}
# plot メソッド
plot.AHP <- function(	obj,					# AHP の手すオブジェクト
			xlab="Score",				# 冯蔡グラフの玻即叹
			main="AHP (Analytic Hierachy Process)",	# 冯蔡グラフの山玛
			file="")				# 冯蔡グラフをファイル叫蜗するときにファイル叹
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

ahp_weight <- function(	x,
			labels.x=NULL,
			labels.y=NULL)
{
	items <- function(n)
	{
		retval <- (1+sqrt(1+8*n))/2
		return(if (retval!=floor(retval)) Inf else retval)
	}
	make.matrix <- function(x)
	{
		n <- items(length(x))
		mat <- diag(n)
		mat[lower.tri(mat, diag=FALSE)] <- x
		mat <- t(mat)+mat
		mat[upper.tri(mat)] <- 1/mat[upper.tri(mat)]
		diag(mat) <- 1
		result <- eigen(mat)
		val <- as.numeric(result$values[1])
		vec <- as.numeric(result$vectors[,1])
		weight <- vec/sum(vec)
		ci <- (val-n)/(n-1)
		cr <- ci/c(0,0,0.58,0.9,1.12,1.24,1.32,1.41,1.45,1.49,1.51,1.53)[n]
    print(cr)
		if (ci > 0.1 || cr > 0.1) {
			cat("\nC.I.=", ci, ",  C.R.=", cr, "\n", sep="")
			print(mat)
			W <- outer(weight, weight, "/")
			print(W)
			print(mat-W)
		}
		return(list(lambda=val, vec=vec, weight=weight, ci=ci, cr=cr, result=result))
	}
	if (is.null(labels.x)) {
		labels.x <- LETTERS[1:items(length(x))]
	}
	ans.x <- make.matrix(x)
	weight.x <- ans.x$weight
	names(weight.x) <- labels.x
  print(weight.x)
  return(ans.x)
}

#x <- c(1/3, 1/5, 1/7, 1/5, 1/7, 1/3)
#y <- matrix(c(1/2,1/3,1/2, 5,2,1/7, 1/3,1/2,2, 2,2,1), 3, 4)
#a <- AHP(x, y, labels.x=c("値段", "燃費", "乗り心地", "車格"), labels.y=c("A 車", "B 車", "C 車"))
#plot(a)

#x <- c(9, 1/3, 5, 1/5, 1/2, 2)
#b <- ahp_weight(x)

construct <- function(x) {
	items <- function(n)
	{
		retval <- (1+sqrt(1+8*n))/2
		return(if (retval!=floor(retval)) Inf else retval)
	}
	make.matrix <- function(x)
	{
		n <- items(length(x))
		mat <- diag(n)
		mat[lower.tri(mat, diag=FALSE)] <- x
		mat <- t(mat)+mat
		mat[upper.tri(mat)] <- 1/mat[upper.tri(mat)]
		diag(mat) <- 1
		result <- eigen(mat)
    #print(mat)
    #print(result)
    return(mat)
  }
	return(make.matrix(x))
}

get_cr <- function(mat) {
	items <- function(n)
	{
		retval <- (1+sqrt(1+8*n))/2
		return(if (retval!=floor(retval)) Inf else retval)
	}
  n <- items(length(x))
  result <- eigen(mat)
  val <- as.numeric(result$values[1])
  vec <- as.numeric(result$vectors[,1])
  weight <- vec/sum(vec)
  ci <- (val-n)/(n-1)
  print(paste("ci = ", ci))
  cr <- ci/c(0,0,0.58,0.9,1.12,1.24,1.32,1.41,1.45,1.49,1.51,1.53)[n]
  return(cr)
}

induced_matrix <- function(m) {
  # 归一化判断矩阵
  for(i in 1:ncol(m)) {
    m[, i] <- m[, i]/colSums(m)[i]
  }
  # 和积法求排序向量w
  w <- matrix(1, nrow(m), 1)
  for(i in 1:nrow(m)) {
    w[i] <- sum(m[i,])/nrow(m) 
    #print(w)
  }
  # 计算诱导矩阵
  for(i in 1:ncol(m)) {
    m[,i] <- m[, i]/w
  }
  return(m)
}

x <- c(1/5, 1/2, 1/4, 1/7, 3, 1/2, 1/2, 2, 1/4, 1/3)
# 需要修改一次CR小于0.1
x <- c(1/3, 1/5, 2)

test <- function(x) {
  m <- construct(x)
  cr <- get_cr(m)
  print(cr)
}

# 需要修改两次CR才能小于0.1
x <- c(8, 1/2, 5, 1/5, 1/2, 2)
test(x)

test1 <- function(x) {
  m <- construct(x)
  cr <- get_cr(m)
  print('Origin Matrix:')
  print(m)
  print('---------------------------------')
  print(cr)
  if(cr > 0.1) {
    print(paste('>>>>> CR:', cr, 'greater than 0.1'))
  }
  while(cr > 0.1) {
    n <- induced_matrix(m)
    c <- sort(n[n>1], decreasing=T)
    print("Induced Matrix:")
    print(n)
    modified <- F
    count <- 0

    for(seq in 1:length(c)) {
      for(i in 1:ncol(n)) {
        for(j in 1:nrow(n)) {
          print(c[seq])
          if(n[i, j] == c[seq] 
              && m[i,j]>1) {

            m[i,j] <- m[i,j] -1
            m[j,i] <- 1/m[i,j]

            print(paste("At position:", i, ",", j))
            print(m)
            cr <- get_cr(m)
            modified <- T
            count <- count + 1
            print(paste('----- CR:', cr, '---------------------------'))
            print(paste('----- Count:', count, '-------'))
          }
          if(cr<0.1 || modified)break
        }
        if(cr<0.1 || modified)break
      }
      if(cr<0.1 || modified)break
    }
  }
}

# 修改前
x <- c(2, 4, 1/2, 3, 2, 3, 1/3, 1/3, 2, 1, 3, 1/3, 1/5, 2, 2, 2, 3, 5, 3, 5, 2)
# 修改后
x <- c(2, 4, 1/2, 3, 2, 3, 1/3, 1/3, 1, 1, 3, 1/3, 1/5, 2, 2, 2, 3, 5, 3, 5, 2)
test1(x)
ahp_weight(x)
y <- c(0.06346093, 0.16532713, 0.16550356, 0.04486344, 0.08807395, 0.17312544, 0.29964553)
p1 <- c(70, 60, 80, 70, 60, 90, 90)
p2 <- c(75, 60, 75, 79, 65, 73, 85)
p3 <- c(90, 85, 90, 65, 60, 65, 70)
print(sum(p1*y))
print(sum(p2*y))
print(sum(p3*y))

a <- c(1/2, 4, 1/3, 3, 1/3, 1/5)
b <- c(1, 6, 4, 2, 1, 1)
test(a)
test(b)
