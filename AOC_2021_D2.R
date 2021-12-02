library(data.table)

Input = fread('C:/Users/noble/Desktop/AOC 2021/AOC_2021_D2.txt', header = F)

#Part 1
sum(Input$V2[Input$V1 == 'forward']) * (sum(Input$V2[Input$V1 == 'down']) - sum(Input$V2[Input$V1 == 'up']))

#Part 2
Input$Aim_Change = ifelse(Input$V1 == 'down', Input$V2, ifelse(Input$V1 == 'up', -Input$V2, 0))
Input$Aim = cumsum(Input$Aim_Change)

Input$Depth_Change = ifelse(Input$V1 == 'forward', Input$V2 * Input$Aim, 0)
Input$Depth = cumsum(Input$Depth_Change)

sum(Input$V2[Input$V1 == 'forward']) * Input$Depth[nrow(Input)]


