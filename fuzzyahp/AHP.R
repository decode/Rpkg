# AHP (Analytic Hierachy Process) §À§Ë§Î…æ≤¡§Úπ‘§¶
AHP <- function(	x,					# …æ≤¡¥Ω‡§ŒΩ≈Õ◊≈Ÿ° ≤ºª∞≥—π‘ŒÛ§Ú•Ÿ•Ø•»•Î§«Õ—∞’°À
			y,					# ¬Â¬ÿ∞∆§Œ…æ≤¡° ≥∆π‘ŒÛ§Œ≤ºª∞≥—π‘ŒÛ§ÚŒÛ§»§π§Îπ‘ŒÛ§«Õ—∞’°À
			labels.x=NULL,				# …æ≤¡¥Ω‡§Œ•È•Ÿ•Î
			labels.y=NULL)				# ¬Â¬ÿ∞∆§Œ•È•Ÿ•Î
{
	items <- function(n)					# ≤ºª∞≥—π‘ŒÛ§ŒÕ◊¡«øÙ§´§Èπ‘ŒÛ•µ•§•∫§Úµ·§·§Î
	{
		retval <- (1+sqrt(1+8*n))/2
		return(if (retval!=floor(retval)) Inf else retval)
	}
	make.matrix <- function(x)				# ¿µ ˝π‘ŒÛ§´§ÈΩ≈§ﬂ•Ÿ•Ø•»•Î§Úµ·§·§Î
	{
		n <- items(length(x))				# π‘ŒÛ§Œ•µ•§•∫
		mat <- diag(n)					# ª∞≥—π‘ŒÛ§Ú…Ω§π•Ÿ•Ø•»•Î§´§Èπ‘ŒÛ§Ú¿∏¿Æ
		mat[lower.tri(mat, diag=FALSE)] <- x
		mat <- t(mat)+mat
		mat[upper.tri(mat)] <- 1/mat[upper.tri(mat)]
		diag(mat) <- 1
		result <- eigen(mat)				# ∏«Õ≠√Õ°¶∏«Õ≠•Ÿ•Ø•»•Î§Úµ·§·§Î
		val <- as.numeric(result$values[1])
		vec <- as.numeric(result$vectors[,1])
		weight <- vec/sum(vec)				# ∏«Õ≠•Ÿ•Ø•»•Î§Úœ¬§¨ 1 §À§ §Î§Ë§¶§À…∏Ω‡≤Ω§∑§ø§‚§Œ§¨Ω≈§ﬂ
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
	if (is.null(labels.x)) {				# •È•Ÿ•Î§¨Õø§®§È§Ï§∆§§§ §§§»§≠§œ A, B, ...
		labels.x <- LETTERS[1:items(length(x))]
	}
	ans.x <- make.matrix(x)
	weight.x <- ans.x$weight				# …æ≤¡¥Ω‡§ŒΩ≈Õ◊≈Ÿ
	names(weight.x) <- labels.x
	nitems.y <- items(nrow(y))
	if (is.null(labels.y)) {				# •È•Ÿ•Î§¨Õø§®§È§Ï§∆§§§ §§§»§≠§œ a, b, ...
		labels.y <- letters[1:nitems.y]
	}
	ans.y <- matrix(unlist(apply(y, 2, make.matrix)), 3+2*nitems.y)
	weight.y <- ans.y[(2+nitems.y):(1+2*nitems.y),]		# ¬Â¬ÿ∞∆§Œ…æ≤¡
	rownames(weight.y) <- labels.y
	colnames(weight.y) <- labels.x
	score <- rowSums(t(weight.x*t(weight.y)))		# •π•≥•¢
	return(structure(list(weight.x=weight.x, weight.y=weight.y, score=score, sorted.score=sort(score)), class="AHP"))
}
# print •·•Ω•√•…
print.AHP <- function(	obj,					# AHP §Œ ÷§π•™•÷•∏•ß•Ø•»
			digits=5)				# ∑Î≤Ã§Œ…Ωº®∑ÂøÙ
{
	cat("\n…æ≤¡¥Ω‡§ŒΩ≈§ﬂ\n\n")
	print(round(obj$weight.x, digits=digits))
	cat("\n¬Â¬ÿ∞∆§Œ…æ≤¡∑Î≤Ã\n\n")
	print(round(obj$weight.y, digits=digits))
	cat("\n•π•≥•¢\n\n")
	print(round(obj$score, digits=digits))
	cat("\n•Ω°º•»§µ§Ï§ø•π•≥•¢\n\n")
	print(round(obj$sorted.score, digits=digits))
}
# plot •·•Ω•√•…
plot.AHP <- function(	obj,					# AHP §Œ ÷§π•™•÷•∏•ß•Ø•»
			xlab="Score",				# ∑Î≤Ã•∞•È•’§Œ≤£º¥Ãæ
			main="AHP (Analytic Hierachy Process)",	# ∑Î≤Ã•∞•È•’§Œ…Ω¬Í
			file="")				# ∑Î≤Ã•∞•È•’§Ú•’•°•§•ÎΩ–Œœ§π§Î§»§≠§À•’•°•§•ÎÃæ
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
		return(list(lambda=val, vec=vec, weight=weight, ci=ci, cr=cr))
	}
	if (is.null(labels.x)) {
		labels.x <- LETTERS[1:items(length(x))]
	}
	ans.x <- make.matrix(x)
	weight.x <- ans.x$weight
	names(weight.x) <- labels.x
  print(weight.x)
}

x <- c(1/3, 1/5, 1/7, 1/5, 1/7, 1/3)
y <- matrix(c(1/2,1/3,1/2, 5,2,1/7, 1/3,1/2,2, 2,2,1), 3, 4)
a <- AHP(x, y, labels.x=c("Çé∂Œ", "»ºŸM", "Å\§Í–ƒµÿ", "‹á∏Ò"), labels.y=c("A ‹á", "B ‹á", "C ‹á"))
plot(a)

x <- c(9, 1, 5, 1/5, 1/2, 2)
b <- ahp_weight(x)
