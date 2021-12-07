options(scipen = 9999)  #disable scientific notation

library(data.table)

Input = data.table(readLines('C:/Users/noble/Desktop/AOC 2021/AOC_2021_D7.txt'))
Input = strsplit(Input$V1, split = ',')
Input = data.table(as.numeric(unlist(Input)))

#Part 1
Fuel = NULL
for (iLevel in 0:max(Input$V1)){
  Fuel = c(Fuel, sum(abs(Input$V1 - iLevel)))
}
Fuel[which(Fuel == min(Fuel))]


#Part 2
Fuel = NULL
for (iLevel in 0:max(Input$V1)){
  Fuel = c(Fuel, sum(abs(Input$V1 - iLevel)*(abs(Input$V1 - iLevel) + 1) / 2))
}
Fuel[which(Fuel == min(Fuel))]
