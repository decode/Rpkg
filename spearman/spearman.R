library(rattle)

sp.correlation <- function(source_file, output_file, threshold, savepic=FALSE) {
  # Load the data.
  crs$dataset <- read.csv(source_file, header=FALSE, na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

  # The following variable selections have been noted.
  crs$input <- c("V1", "V2", "V3", "V4",
                 "V5", "V6", "V7", "V8",
                 "V9", "V10", "V11", "V12",
                 "V13", "V14", "V15", "V16",
                 "V17", "V18", "V19", "V20")

  crs$numeric <- c("V1", "V2", "V3", "V4",
                   "V5", "V6", "V7", "V8",
                   "V9", "V10", "V11", "V12",
                   "V13", "V14", "V15", "V16",
                   "V17", "V18", "V19", "V20")

  crs$categoric <- NULL
  crs$target  <- NULL
  crs$risk    <- NULL
  crs$ident   <- NULL
  crs$ignore  <- NULL
  crs$weights <- NULL

  # Generate a correlation plot for the variables. 
  # The 'ellipse' package provides the 'plotcorr' function.
  require(ellipse, quietly=TRUE)

  # Correlations work for numeric variables only.
  crs$cor <- cor(crs$dataset[, crs$numeric], use="pairwise", method="spearman")

  # Order the correlations by their strength.
  #crs$ord <- order(crs$cor[1,])
  #crs$cor <- crs$cor[crs$ord, crs$ord]

  # Display the actual correlations.
  #print(crs$cor)

  write.csv(crs$cor, paste(output_file, '.csv')) 

  if(savepic) {
    # Graphically display the correlations.
    plotcorr(crs$cor, col=colorRampPalette(c("red", "white", "blue"))(11)[5*crs$cor + 6])
    title(main="Correlation 1_dm.csv using Spearman", sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

  }
  else { 
    # Generate hierarchical cluster of variables.
    hc <- hclust(dist(crs$cor), method="average")

    # Generate the dendrogram.
    dn <- as.dendrogram(hc)

    # Now draw the dendrogram.
    op <- par(mar = c(3, 4, 3, 1.43))
    plot(dn, horiz = TRUE, nodePar = list(col = 3:2, cex = c(2.0, 0.75), pch = 21:22, bg=  c("light blue", "pink"), lab.cex = 0.75, lab.col = "tomato"), edgePar = list(col = "gray", lwd = 2), xlab="Height")
    title(main="Variable Correlation Clusters
          2_nu.csv using Spearman",
          sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
    par(op)
  }
  library(cairoDevice)
  pic_file <- paste(output_file, ".png")
  savePlotToFile(pic_file, 2)

  print(crs$cor)
  weight <- sp.stat(crs$cor, threshold)
  #ds <- sp.transform(crs$dataset)
  #write.csv(ds, paste(output_file, 'trans.csv'))
  theta <- sp.theta(crs$dataset, weight)
  write.csv(theta, paste(output_file, 'theta.csv'))
}

# 
sp.stat <- function(cor, threshold) {
  wei <- matrix(0, nrow(cor), 1)
  # Count number larger than param threshold
  for(i in 1:nrow(cor)) {
    wei[i] <- sum(cor[i,] > threshold) - 1
    print(wei[i])
  }
  total <- sum(wei)
  if(total != 0) {
    for(i in 1:length(wei)) {
      wei[i] <- wei[i]/total
    }
  }
  print('=============== Weight ===============')
  print(wei)
  return(wei)
}

# transform matrix's value 1,2,3,4 to 1,2,4,8
sp.transform <- function(dataset) {
  res <- dataset
  for(i in 1:nrow(dataset)) {
    for(j in 1:ncol(dataset)) {
      res[i,j] <- switch(dataset[i,j], 1, 2, 4, 8)
    }
  }
  #print(res)
  return(res)
}

# formula: theta = sum(row[i] * weight)/ sum(theta)
sp.theta <- function(dataset, weight) {
  if(ncol(dataset) != length(weight)) {
    print('dataset not match weight array')
    return
  }
  wei <- matrix(0, nrow(dataset), 1)
  for(i in 1:nrow(dataset)) {
    for(j in 1:ncol(dataset)) {
      wei[i] <- dataset[i, j] * weight[j]
    }
  }
  total <- sum(wei)
  if(total != 0) {
    for(i in 1:length(wei)) {
      wei[i] <- wei[i]/total
    }
  }
  print('=============== Theta ===============')
  print(wei)
  return(wei)
}

sp.caculate <- function() {
}

#sp.correlation("file:///home/home/1_dm.csv", "res_dm", 0.85, F)
#sp.correlation("file:///home/home/1_eu.csv", "res_eu", 0.9, F)
#sp.correlation("file:///home/home/1_nu.csv", "res_nu", 0.9, F)
sp.correlation("file:///home/home/test.csv", "res_test", 0.8, F)
