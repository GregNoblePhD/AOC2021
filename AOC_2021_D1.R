library(data.table)

Input = fread('C:/Users/noble/Desktop/AOC 2021/AOC_2021_D1.txt')

#Part 1
Input$Prev = c(NA, Input$V1[1:(nrow(Input)-1)])
Input$Increase = ifelse(Input$V1 > Input$Prev, 1, 0)
sum(Input$Increase, na.rm = T)

#Part 2
Input$Three = Input$V1 + c(Input$V1[2:nrow(Input)], NA) + c(Input$V1[3:nrow(Input)], NA, NA)
Input$Prev_Three = c(NA, Input$Three[1:(nrow(Input)-1)])
Input$Increase_Three = ifelse(Input$Three > Input$Prev_Three, 1, 0)
sum(Input$Increase_Three, na.rm = T)


