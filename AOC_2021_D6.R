options(scipen = 9999)  #disable scientific notation

library(data.table)
library(dplyr)

Input = data.table(readLines('C:/Users/noble/Desktop/AOC 2021/AOC_2021_D6.txt'))
Input = strsplit(Input$V1, split = ',')
Input = data.table(as.numeric(unlist(Input)))


#Part 1 & 2
nDays = 256
Mat = c(rep(0,9))

X = data.table(Input %>%
               group_by(V1) %>%
               summarise(n = n()))

for(iDay in 0:8){
  if(any(X$V1 == iDay)){
    Mat[iDay+1] = X$n[X$V1 == iDay]
  }
}

for (iDay in 1:nDays){
  Zeros = Mat[1]
  Mat = c(Mat[2:9], Zeros)
  Mat[7] = Mat[7] + Zeros
}

sum(Mat)


