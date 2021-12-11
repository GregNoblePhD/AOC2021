options(scipen = 9999)  #disable scientific notation

library(data.table)

Input = fread('C:/Users/noble/Desktop/AOC 2021/AOC_2021_D11.txt', header = F, keepLeadingZeros = T, fill=TRUE)
Input$V1 = as.character(Input$V1)
x = strsplit(Input$V1, '')


#Part 1
Mat = matrix(0, nrow = 10, ncol = 10)

for (iRow in 1:10){
  Mat[iRow,] = as.numeric(x[[iRow]])
}

Tot_Lit = 0
Steps = 100
for(nStep in 1:Steps){
  Already_Lit = NULL
  Mat = Mat + 1
  Open_Light = which(Mat > 9)
  while(length(Open_Light) > 0){
    for (nLight in Open_Light){
      if(nLight-1 > 0 & (nLight-1) %% 10 != 0) {Mat[nLight-1] = Mat[nLight-1] + 1}
      if(nLight+1 < 101 & nLight %% 10 != 0) {Mat[nLight+1] = Mat[nLight+1] + 1}
      if(nLight-10 > 0) {Mat[nLight-10] = Mat[nLight-10] + 1}
      if(nLight+10 < 101) {Mat[nLight+10] = Mat[nLight+10] + 1}
      if(nLight-11 > 0 & (nLight-1) %% 10 != 0) {Mat[nLight-11] = Mat[nLight-11] + 1}
      if(nLight+11 < 101 & nLight %% 10 != 0) {Mat[nLight+11] = Mat[nLight+11] + 1}
      if(nLight-9 > 0 & nLight %% 10 != 0) {Mat[nLight-9] = Mat[nLight-9] + 1}
      if(nLight+9 < 101 & (nLight-1) %% 10 != 0) {Mat[nLight+9] = Mat[nLight+9] + 1}
    }
    Already_Lit = c(Already_Lit, Open_Light)
    Open_Light = which(Mat > 9)[!(which(Mat > 9) %in% Already_Lit)]
  }
  Tot_Lit = Tot_Lit + length(which(Mat > 9))
  Mat[Mat > 9] = 0
}
Tot_Lit



#Part 2
Mat = matrix(0, nrow = 10, ncol = 10)

for (iRow in 1:10){
  Mat[iRow,] = as.numeric(x[[iRow]])
}

Steps = 1000
for(nStep in 1:Steps){
  Already_Lit = NULL
  Mat = Mat + 1
  Open_Light = which(Mat > 9)
  while(length(Open_Light) > 0){
    for (nLight in Open_Light){
      if(nLight-1 > 0 & (nLight-1) %% 10 != 0) {Mat[nLight-1] = Mat[nLight-1] + 1}
      if(nLight+1 < 101 & nLight %% 10 != 0) {Mat[nLight+1] = Mat[nLight+1] + 1}
      if(nLight-10 > 0) {Mat[nLight-10] = Mat[nLight-10] + 1}
      if(nLight+10 < 101) {Mat[nLight+10] = Mat[nLight+10] + 1}
      if(nLight-11 > 0 & (nLight-1) %% 10 != 0) {Mat[nLight-11] = Mat[nLight-11] + 1}
      if(nLight+11 < 101 & nLight %% 10 != 0) {Mat[nLight+11] = Mat[nLight+11] + 1}
      if(nLight-9 > 0 & nLight %% 10 != 0) {Mat[nLight-9] = Mat[nLight-9] + 1}
      if(nLight+9 < 101 & (nLight-1) %% 10 != 0) {Mat[nLight+9] = Mat[nLight+9] + 1}
    }
    Already_Lit = c(Already_Lit, Open_Light)
    Open_Light = which(Mat > 9)[!(which(Mat > 9) %in% Already_Lit)]
  }
  if(length(which(Mat > 9)) == 100){break}
  Mat[Mat > 9] = 0
}
nStep


