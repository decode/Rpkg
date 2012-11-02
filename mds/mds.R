# Example 1
library(ggplot2)
data(watervoles, package = "HSAUR2")
data(watervoles)
voles.mds=cmdscale(watervoles,k=13,eig=T)

sum(abs(voles.mds$eig[1:2]))/sum(abs(voles.mds$eig))
sum((voles.mds$eig[1:2])^2)/sum((voles.mds$eig)^2)

x = voles.mds$points[,1]
y = voles.mds$points[,2]
p=ggplot(data.frame(x,y),aes(x,y,label = colnames(watervoles)))
p+geom_point(shape=16,size=3,colour='red')+geom_text(hjust=-0.1,vjust=0.5,alpha=0.5)

# Example 2
library("MASS")
data(voting, package = "HSAUR2")
voting_mds = isoMDS(voting)
x = voting_mds$points[,1]
y = voting_mds$points[,2]
g=ggplot(data.frame(x,y),aes(x,y,label = colnames(voting)))
g+geom_point(shape=16,size=3,colour='red')+geom_text(hjust=-0.1,vjust=0.5,alpha=0.5)

# Example 3 http://stat.ethz.ch/R-manual/R-patched/library/MASS/html/isoMDS.html
swiss.x <- as.matrix(swiss[, -1])
swiss.dist <- dist(swiss.x)
swiss.mds <- isoMDS(swiss.dist)
plot(swiss.mds$points, type = "n")
text(swiss.mds$points, labels = as.character(1:nrow(swiss.x)))
swiss.sh <- Shepard(swiss.dist, swiss.mds$points)
plot(swiss.sh, pch = ".")
lines(swiss.sh$x, swiss.sh$yf, type = "S")

# --------------------------------------------------------

library('gdata')
xls <- read.xls('~/develop/git/aubluo/tools/fa.xlsx', sheet=1, header=F)
xls <- xls[5:215,]

# 分别对评价者和变量进行划分,计算距离
d <- dist(xls, method='euclidean')
d <- dist(t(as.matrix(xls)), method='euclidean')

# 绘制热图
heatmap(as.matrix(d),labRow = F, labCol = F)
# 使用hclust函数建立聚类模型
model1=hclust(d,method='ward')
result=cutree(model1,k=5)
result

# 经典MDS --------------------------------------------
x_mds = cmdscale(as.matrix(d), k=2, eig=T)
# 拟合优度,拟合优度（Goodness of Fit）是指回归直线对观测值的拟合程度
x_mds$GOF


# 非度量MDS ------------------------------------------
x_mds = isoMDS(as.matrix(d))
# stress越低越好?
x_mds$stress

sum(abs(x_mds$eig[1:2]))/sum(abs(x_mds$eig))
sum((x_mds$eig[1:2])^2)/sum((x_mds$eig)^2)
x = x_mds$points[,1]
y = x_mds$points[,2]

g=ggplot(data.frame(x,y),aes(x,y,label = rownames(xls)))
g+geom_point(shape=16,size=3,colour='red')+geom_text(hjust=-0.1,vjust=0.5,alpha=0.5)

p=ggplot(data.frame(x,y),aes(x,y))
p+geom_point(size=3,alpha=0.8, aes(colour=factor(result)))

# -----------------------------------------------------

# 非度量的mds方法
mds.iso <- function(sheetnum=1, h=F) {
  xls <- read.xls('~/develop/git/aubluo/tools/fa.xlsx', sheet=sheetnum, header=h)

  # 分别对评价者和变量进行划分,计算距离
  d <- dist(xls, method='euclidean')
  x_mds = isoMDS(as.matrix(d))

  x = x_mds$points[,1]
  y = x_mds$points[,2]

  g=ggplot(data.frame(x,y),aes(x,y,label = rownames(xls)))
  g+geom_point(shape=16,size=3,colour='red')+geom_text(hjust=-0.1,vjust=0.5,alpha=0.5)
}

mds.iso()
