options(scipen = 9999)  #disable scientific notation

library(data.table)

Input = do.call(rbind, strsplit(readLines('C:/Users/noble/Desktop/AOC 2021/AOC_2021_D25.txt'), ''))

Mat_Rows = nrow(Input)
Mat_Cols = ncol(Input)

Step = 0
Any_Moves = 1
while(Any_Moves != 0){
  Step = Step + 1
  Any_Moves = 0
  
  #Identify the east herds and the spots that they are atempting to move to
  East_Herd = data.table(which(Input == '>', arr.ind = T))
  Open_Spot = data.table(which(Input == '.', arr.ind = T))
  Open_Spot$Prev_Col = ifelse(Open_Spot$col == 1, Mat_Cols, Open_Spot$col - 1)
  setnames(Open_Spot, c('row', 'Next', 'col'))
  
  #check for feasibility of the move
  East_Check = merge(East_Herd, Open_Spot, by = c('row', 'col'), all.x = T)
  East_Check = East_Check[!is.na(Next)]
  
  #if any moves exist, mark loop for continuation and update the Input object
  if(nrow(East_Check) > 0){
    Any_Moves = 1
    for(iRow in 1:nrow(East_Check)){
      Input[East_Check$row[iRow], East_Check$col[iRow]] = '.'
      Input[East_Check$row[iRow], East_Check$Next[iRow]] = '>'
    }
  }
  
  #Identify the south herds and the spots that they are atempting to move to
  South_Herd = data.table(which(Input == 'v', arr.ind = T))
  Open_Spot = data.table(which(Input == '.', arr.ind = T))
  Open_Spot$Prev_Row = ifelse(Open_Spot$row == 1, Mat_Rows, Open_Spot$row - 1)
  setnames(Open_Spot, c('Next', 'col', 'row'))
  
  #check for feasibility of the move
  South_Check = merge(South_Herd, Open_Spot, by = c('row', 'col'), all.x = T)
  South_Check = South_Check[!is.na(Next)]
  
  #if any moves exist, mark loop for continuation and update the Input object
  if(nrow(East_Check) > 0){
    Any_Moves = 1
    for(iRow in 1:nrow(South_Check)){
      Input[South_Check$row[iRow], South_Check$col[iRow]] = '.'
      Input[South_Check$Next[iRow], South_Check$col[iRow]] = 'v'
    }
  }
}
Step  #Result
