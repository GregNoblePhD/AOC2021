options(scipen = 9999)  #disable scientific notation

library(data.table)

Input = fread('C:/Users/noble/Desktop/AOC 2021/AOC_2021_D8.txt', header = F, keepLeadingZeros = T, fill=TRUE)

#Part 1
sum(nchar(c(Input$V12, Input$V13, Input$V14, Input$V15)) %in% c(2,4,3,7))

#Part 2
Result = 0
for (iEnt in 1:nrow(Input)){
  temp_Input = unlist(Input[iEnt, 1:10])
  temp_Target = Input[iEnt, 12:15]
  temp_1 = unname(temp_Input[which(nchar(temp_Input) == 2)])
  temp_7 = unname(temp_Input[which(nchar(temp_Input) == 3)])
  temp_4 = unname(temp_Input[which(nchar(temp_Input) == 4)])
  temp_8 = unname(temp_Input[which(nchar(temp_Input) == 7)])
  Pos_1 = setdiff(strsplit(temp_7, '')[[1]], strsplit(temp_1, '')[[1]])
  Pos_2_4 = setdiff(strsplit(temp_4, '')[[1]], strsplit(temp_1, '')[[1]])

  temp_nchar_6 = unname(temp_Input[which(nchar(temp_Input) == 6)])
  if(length(setdiff(Pos_2_4, strsplit(temp_nchar_6, '')[[1]])) > 0){
    temp_0 = temp_nchar_6[1]
    Pos_4 = setdiff(Pos_2_4, strsplit(temp_nchar_6, '')[[1]])
    }
  if(length(setdiff(Pos_2_4, strsplit(temp_nchar_6, '')[[2]])) > 0){
    temp_0 = temp_nchar_6[2]
    Pos_4 = setdiff(Pos_2_4, strsplit(temp_nchar_6, '')[[2]])
    }
  if(length(setdiff(Pos_2_4, strsplit(temp_nchar_6, '')[[3]])) > 0){
    temp_0 = temp_nchar_6[3]
    Pos_4 = setdiff(Pos_2_4, strsplit(temp_nchar_6, '')[[3]])
  }
  
  Pos_2 = setdiff(Pos_2_4, Pos_4)
  
  temp_nchar_5 = unname(temp_Input[which(nchar(temp_Input) == 5)])
  if(Pos_2 %in% strsplit(temp_nchar_5, '')[[1]]){
    temp_5 = temp_nchar_5[1]
  }
  if(Pos_2 %in% strsplit(temp_nchar_5, '')[[2]]){
    temp_5 = temp_nchar_5[2]
  }
  if(Pos_2 %in% strsplit(temp_nchar_5, '')[[3]]){
    temp_5 = temp_nchar_5[3]
  }
  Pos_3 = setdiff(strsplit(temp_1, '')[[1]],  unname(strsplit(temp_5, ''))[[1]])
  Pos_6 = setdiff(strsplit(temp_1, '')[[1]], Pos_3)
 
  
  if(!(Pos_3 %in% strsplit(temp_nchar_6, '')[[1]])){
    temp_6 = temp_nchar_6[1]
  }
  if(!(Pos_3 %in% strsplit(temp_nchar_6, '')[[2]])){
    temp_6 = temp_nchar_6[2]
  }
  if(!(Pos_3 %in% strsplit(temp_nchar_6, '')[[3]])){
    temp_6 = temp_nchar_6[3]
  } 
  temp_9 = setdiff(temp_nchar_6, c(temp_0, temp_6))
  
  if(!(Pos_6 %in% strsplit(temp_nchar_5, '')[[1]])){
    temp_2 = temp_nchar_5[1]
  }
  if(!(Pos_6 %in% strsplit(temp_nchar_5, '')[[2]])){
    temp_2 = temp_nchar_5[2]
  }
  if(!(Pos_6 %in% strsplit(temp_nchar_5, '')[[3]])){
    temp_2 = temp_nchar_5[3]
  } 
  temp_3 = setdiff(temp_nchar_5, c(temp_2, temp_5))
  
  Key = data.table('Value' = 0:9, 'Code' = c(temp_0, temp_1, temp_2, temp_3, 
                                              temp_4, temp_5, temp_6, temp_7, 
                                              temp_8, temp_9))
  Key$Code_Sort = sapply(strsplit(Key$Code, ''), function(x){ paste0(sort(x), collapse = '')})
  
  Result = Result + as.numeric(paste0(Key$Value[Key$Code_Sort == paste0(sort(unlist(strsplit(temp_Target$V12[1], ''))), collapse = '')],
                                Key$Value[Key$Code_Sort == paste0(sort(unlist(strsplit(temp_Target$V13[1], ''))), collapse = '')],
                                Key$Value[Key$Code_Sort == paste0(sort(unlist(strsplit(temp_Target$V14[1], ''))), collapse = '')],
                                Key$Value[Key$Code_Sort == paste0(sort(unlist(strsplit(temp_Target$V15[1], ''))), collapse = '')]
  ))
  
}
Result





