library(data.table)

#Read and Format Data
Input = data.table(readLines('C:/Users/noble/Desktop/AOC 2021/AOC_2021_D5.txt'))
Input$V1 = gsub(' -> ', ',', Input$V1)
Input = strsplit(Input$V1, split = ',')
Input = data.table(cbind('x1' = as.numeric(sapply(Input, "[[", 1)), 
                     'y1' = as.numeric(sapply(Input, "[[", 2)),
                     'x2' = as.numeric(sapply(Input, "[[", 3)), 
                     'y2' = as.numeric(sapply(Input, "[[", 4))))


#Part 1  Only horizontal or vertical lines
max_X = max(c(max(Input$x1), max(Input$x2)))
max_Y = max(c(max(Input$y1), max(Input$y2)))

Mat = matrix(0, nrow = max(c(max_X, max_Y))+1, ncol = max(c(max_X, max_Y))+1)

for (iLine in 1:nrow(Input)){
  tX1 = Input$x1[iLine]
  tY1 = Input$y1[iLine]
  tX2 = Input$x2[iLine]
  tY2 = Input$y2[iLine]
  
  if(tX1 == tX2) {
    if (tY1 > tY2){Y = tY2:tY1}
    if (tY1 < tY2){Y = tY1:tY2}
    for (i in Y){
      Mat[i+1, tX1+1] = Mat[i+1, tX1+1]+1
    }
  } 
  if (tY1 == tY2){
    if (tX1 > tX2){X = tX2:tX1}
    if (tX1 < tX2){X = tX1:tX2}
    for (i in X){
      Mat[tY1+1, i+1] = Mat[tY1+1, i+1]+1
    }
  }
}

length(which(Mat > 1))


#Part 2  #same but add in the diagonals
max_X = max(c(max(Input$x1), max(Input$x2)))
max_Y = max(c(max(Input$y1), max(Input$y2)))

Mat = matrix(0, nrow = max(c(max_X, max_Y))+1, ncol = max(c(max_X, max_Y))+1)

for (iLine in 1:nrow(Input)){
  tX1 = Input$x1[iLine]
  tY1 = Input$y1[iLine]
  tX2 = Input$x2[iLine]
  tY2 = Input$y2[iLine]
  
  if(tX1 == tX2) {
    if (tY1 > tY2){Y = tY2:tY1}
    if (tY1 < tY2){Y = tY1:tY2}
    for (i in Y){
      Mat[i+1, tX1+1] = Mat[i+1, tX1+1]+1
    }
  } 
  if (tY1 == tY2){
    if (tX1 > tX2){X = tX2:tX1}
    if (tX1 < tX2){X = tX1:tX2}
    for (i in X){
      Mat[tY1+1, i+1] = Mat[tY1+1, i+1]+1
    }
  }
  if (tX1 != tX2 & tY1 != tY2){
    X = tX1:tX2
    Y = tY1:tY2
    for (i in 1:length(X)){
      Mat[Y[i]+1, X[i]+1] = Mat[Y[i]+1, X[i]+1]+1
    }
  }
}

length(which(Mat > 1))
