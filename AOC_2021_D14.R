options(scipen = 9999)  #disable scientific notation

#libraries
library(data.table)
library(stringr)
library(dplyr)

#Read in the data
Input = data.table(readLines('C:/Users/noble/Desktop/AOC 2021/AOC_2021_D14.txt'))

#Pull the code from the rules and determine the last letter of the code
Start_Code = Input$V1[1]
Last_Letter = substr(Start_Code, nchar(Start_Code), nchar(Start_Code))
Rules = Input[3:nrow(Input),]

#Build out the Rules DT to track counts of each segment
Rules = strsplit(Rules$V1, ' -> ')
Rules = data.table(cbind('Code' = sapply(Rules, "[[", 1), 
                        'Insert' = sapply(Rules, "[[", 2)))
Rules$First_Replace = paste0(substr(Rules$Code, 1, 1), Rules$Insert, substr(Rules$Code, 2, 2))
Rules$Replace1 = paste0(substr(Rules$Code, 1, 1), Rules$Insert)
Rules$Replace2 = paste0(Rules$Insert, substr(Rules$Code, 2, 2))
Rules$Let1 = substr(Rules$Code, 1, 1)
Rules$Let2 = substr(Rules$Code, 2, 2)
Rules$Val = 0

#Populate the initial counts
Split_Code = strsplit(Start_Code, '')[[1]]
Pairs = paste0(lag(strsplit(Start_Code, '')[[1]]), Split_Code)
Pairs = Pairs[2:length(Pairs)]
for(iPair in Pairs){
  Rules$Val[Rules$Code == iPair] = Rules$Val[Rules$Code == iPair] + 1
}

#function to itereate through the required steps
Expand_Code = function(Rules, Steps){
  for (iStep in 1:Steps){
    Rules$temp_Val = 0
    for (iRow in 1:nrow(Rules)){
      if(Rules$Val[iRow] > 0){
        Rules$temp_Val[which(Rules$Code == Rules$Replace1[iRow])] = Rules$temp_Val[which(Rules$Code == Rules$Replace1[iRow])] +
                                                               Rules$Val[iRow]
        Rules$temp_Val[which(Rules$Code == Rules$Replace2[iRow])] = Rules$temp_Val[which(Rules$Code == Rules$Replace2[iRow])] +
                                                               Rules$Val[iRow]
      }
    }
    Rules$Val = Rules$temp_Val
  }
  
  Char_List = unique(c(Rules$Let1, Rules$Let2))
  Result = NULL
  for(iPat in Char_List){
    Result = rbindlist(list(Result, data.table('Letter' = iPat, 
                                               'Val' = sum(Rules$Val[Rules$Let1 == iPat]))))
  }
  Result$Val[Result$Letter == Last_Letter] = Result$Val[Result$Letter == Last_Letter] + 1
  return(max(Result$Val) - min(Result$Val))
}

#Part 1
Expand_Code(Rules, 10)

#Part 2
Expand_Code(Rules, 40)

