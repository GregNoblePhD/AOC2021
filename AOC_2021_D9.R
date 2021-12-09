options(scipen = 9999)  #disable scientific notation

library(data.table)

Input = fread('C:/Users/noble/Desktop/AOC 2021/AOC_2021_D9.txt', header = F, keepLeadingZeros = T, fill=TRUE)

Input$V1 = as.character(Input$V1)
x = strsplit(Input$V1, '')
Rows = nrow(Input)
Cols = nchar(Input$V1[1])

Basin_Check = function(pt_Val, xRow, xCol){
  val1 = NULL
  val2 = NULL
  val3 = NULL
  val4 = NULL
  if(xRow > 1){
    up_pt = as.numeric(x[[xRow-1]][xCol])
    if(pt_Val < up_pt & up_pt != 9){
      tval1 = Basin_Check(up_pt, xRow-1, xCol)
      val1= c(paste(xRow-1, xCol, sep = ','), tval1)
    }
  }
  
  if(xRow < Rows){
    down_pt = as.numeric(x[[xRow+1]][xCol])
    if(pt_Val < down_pt & down_pt != 9){
      tval2 = Basin_Check(down_pt, xRow+1, xCol)
      val2 = c(paste(xRow+1, xCol, sep = ','), tval2)
    }
  } 
  
  if(xCol > 1){
    left_pt = as.numeric(x[[xRow]][xCol-1])
    if(pt_Val < left_pt & left_pt != 9){
      tval3 = Basin_Check(left_pt, xRow, xCol-1)
      val3 = c(paste(xRow, xCol-1, sep = ','), tval3)
    }
  } 
  
  if(xCol < Cols){
    right_pt = as.numeric(x[[xRow]][xCol+1])
    if(pt_Val < right_pt & right_pt != 9){
      tval4 = Basin_Check(right_pt, xRow, xCol+1)
      val4 = c(paste(xRow, xCol+1, sep = ','), tval4)
    }
  } 
  
  return (c(val1, val2, val3, val4))
}

#Part 1
Low_Points = NULL
for (iRow in 1:Rows){
  for (iCol in 1:Cols){
    temp_pt = as.numeric(x[[iRow]][iCol])
    if(iRow > 1){
      up_pt = as.numeric(x[[iRow-1]][iCol])
    } else { up_pt = 10 }
    
    if(iRow < Rows){
      down_pt = as.numeric(x[[iRow+1]][iCol])
    } else { down_pt = 10 }
    
    if(iCol > 1){
      left_pt = as.numeric(x[[iRow]][iCol-1])
    } else {left_pt = 10}
    
    if(iCol < Cols){
      right_pt = as.numeric(x[[iRow]][iCol+1])
    } else {right_pt = 10}
    
    if(temp_pt < up_pt & temp_pt < down_pt & temp_pt < left_pt & temp_pt < right_pt){
      Low_Points = c(Low_Points, as.numeric(temp_pt) + 1)
    }
  }
}

sum(Low_Points)


#Part 2
Basin_Points = NULL
for (iRow in 1:Rows){
  for (iCol in 1:Cols){
    temp_pt = as.numeric(x[[iRow]][iCol])
    if(iRow > 1){
      up_pt = as.numeric(x[[iRow-1]][iCol])
    } else { up_pt = 10 }
    
    if(iRow < Rows){
      down_pt = as.numeric(x[[iRow+1]][iCol])
    } else { down_pt = 10 }
    
    if(iCol > 1){
      left_pt = as.numeric(x[[iRow]][iCol-1])
    } else {left_pt = 10}
    
    if(iCol < Cols){
      right_pt = as.numeric(x[[iRow]][iCol+1])
    } else {right_pt = 10}
    
    if(temp_pt < up_pt & temp_pt < down_pt & temp_pt < left_pt & temp_pt < right_pt){
      Basin_Points = c(Basin_Points, length(unique(Basin_Check(temp_pt, iRow, iCol))) + 1)
    }
  }
}

Basin_Points = Basin_Points[order(Basin_Points)]
Basin_Points[length(Basin_Points)] * Basin_Points[length(Basin_Points)-1] * Basin_Points[length(Basin_Points)-2]

