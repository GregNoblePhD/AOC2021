options(scipen = 9999)  #disable scientific notation

library(data.table)

Input = data.table(readLines('C:/Users/noble/Desktop/AOC 2021/AOC_2021_D20.txt'))
#Input = data.table(readLines('C:/Users/noble/Desktop/AOC 2021/Test.txt'))

Input_Image = Input[3:nrow(Input),]
IEA = strsplit(Input$V1[1], '')[[1]]

Bit_to_Integer = function(Bin_Val) {
  return(strtoi(paste(Bin_Val, collapse = ''), base = 2))
}

Val_Check = function(x, y){
  Cur_Col = y
  Cur_Row = x
  Row_Check_Up = ifelse(Cur_Row - 1 == 0, 0, 1)
  Row_Check_Down = ifelse(Cur_Row >= nrow(Output_Mat), 0, 1)
  Col_Check_Left = ifelse(Cur_Col - 1 == 0, 0, 1)
  Col_Check_Right = ifelse(Cur_Col >= ncol(Output_Mat), 0, 1)
  
  Cur_String = paste0(ifelse(Row_Check_Up == 1 & Col_Check_Left == 1, Output_Mat[Cur_Row - 1, Cur_Col - 1], 0),
                      ifelse(Row_Check_Up == 1,  Output_Mat[Cur_Row - 1, Cur_Col], 0),
                      ifelse(Row_Check_Up == 1 & Col_Check_Right == 1, Output_Mat[Cur_Row - 1, Cur_Col + 1], 0),
                      ifelse(Col_Check_Left == 1, Output_Mat[Cur_Row, Cur_Col - 1], 0),
                      Output_Mat[Cur_Row, Cur_Col],
                      ifelse(Col_Check_Right == 1, Output_Mat[Cur_Row, Cur_Col + 1], 0),
                      ifelse(Row_Check_Down == 1 & Col_Check_Left == 1, Output_Mat[Cur_Row + 1, Cur_Col - 1], 0),
                      ifelse(Row_Check_Down == 1, Output_Mat[Cur_Row + 1, Cur_Col], 0),
                      ifelse(Row_Check_Down == 1 & Col_Check_Right == 1, Output_Mat[Cur_Row + 1, Cur_Col + 1], 0))
  return(ifelse(IEA[Bit_to_Integer(Cur_String)+1] == '#', 1, 0))
}

#Part 1
Offset = 20
Output_Mat = matrix(0, nrow = nrow(Input_Image) + Offset, ncol = nchar(Input_Image$V1[1]) + Offset)
for(iRow in 1:nrow(Input_Image)){
  for(iCol in 1:nchar(Input_Image$V1[1])){
    Output_Mat[iRow + Offset/2, iCol + Offset/2] = ifelse(substr(Input_Image$V1[iRow], iCol, iCol) == '#', 1, 0)
  }
}

Steps = 2
for (iStep in 1:Steps){
  New_Mat = Output_Mat
  for(iX in 1:(ncol(New_Mat))){
    for(iY in 1:(nrow(New_Mat))){
      New_Mat[iX, iY] = Val_Check(iX, iY)
    }
  }
  if(iStep %% 2 == 0){
    New_Mat[1,] = 0
    New_Mat[nrow(New_Mat),] = 0
    New_Mat[,1] = 0
    New_Mat[ncol(New_Mat),] = 0
  } else {
    New_Mat[1,] = 1
    New_Mat[nrow(New_Mat),] = 1
    New_Mat[,1] = 1
    New_Mat[,ncol(New_Mat)] = 1
  }
  Output_Mat = New_Mat
}  
sum(New_Mat)



#Part 2
Offset = 120
Output_Mat = matrix(0, nrow = nrow(Input_Image) + Offset, ncol = nchar(Input_Image$V1[1]) + Offset)
for(iRow in 1:nrow(Input_Image)){
  for(iCol in 1:nchar(Input_Image$V1[1])){
    Output_Mat[iRow + Offset/2, iCol + Offset/2] = ifelse(substr(Input_Image$V1[iRow], iCol, iCol) == '#', 1, 0)
  }
}

Steps = 50
for (iStep in 1:Steps){
  New_Mat = Output_Mat
  for(iX in 1:(ncol(New_Mat))){
    for(iY in 1:(nrow(New_Mat))){
      New_Mat[iX, iY] = Val_Check(iX, iY)
    }
  }
  if(iStep %% 2 == 0){
    New_Mat[1,] = 0
    New_Mat[nrow(New_Mat),] = 0
    New_Mat[,1] = 0
    New_Mat[ncol(New_Mat),] = 0
  } else {
    New_Mat[1,] = 1
    New_Mat[nrow(New_Mat),] = 1
    New_Mat[,1] = 1
    New_Mat[,ncol(New_Mat)] = 1
  }
  Output_Mat = New_Mat
  print(paste0('Step ', iStep, ' Done'))
}  
sum(New_Mat)

