library(data.table)

Input = fread('C:/Users/noble/Desktop/AOC 2021/AOC_2021_D4.txt', header = F, keepLeadingZeros = T, fill=TRUE)
Draws = c(42,32,13,22,91,2,88,85,53,87,37,33,76,98,89,19,69,9,62,21,38,49,54,81,0,26,79,36,57,18,4,40,31,80,24,64,77,97,70,6,73,23,20,47,45,51,74,25,95,96,58,92,94,11,39,63,65,99,48,83,29,34,44,75,55,17,14,56,8,82,59,52,46,90,5,41,60,67,16,1,15,61,71,66,72,30,28,3,43,27,78,10,86,7,50,35,84,12,93,68)

Board_Size = 5
Board_Count = ceiling(nrow(Input) / (Board_Size + 1))

#Part 1
Input$Row = 1:nrow(Input)
Input$Board = ceiling(Input$Row / (Board_Size + 1))
Input = Input[!is.na(V1),]

Result = NULL
for(nDraw in Draws){
  
  Input$V1[Input$V1 == nDraw] = -1
  Input$V2[Input$V2 == nDraw] = -1
  Input$V3[Input$V3 == nDraw] = -1
  Input$V4[Input$V4 == nDraw] = -1
  Input$V5[Input$V5 == nDraw] = -1
  
  for (nBoard in unique(Input$Board)){
    TempB = Input[Board == nBoard]
    TempB = as.matrix(TempB[,list(V1, V2, V3, V4, V5)])
    if (any(rowSums(TempB) == -Board_Size) | any(colSums(TempB) == -Board_Size)){
      Result = sum(TempB[which(TempB != -1)], na.rm = T) * nDraw
      break
    }
  }
  
  if(!is.null(Result)){
    break
  }
}

Result


#Part 2

Input = fread('C:/Users/noble/Desktop/AOC 2021/AOC_2021_D4.txt', header = F, keepLeadingZeros = T, fill=TRUE)
Draws = c(42,32,13,22,91,2,88,85,53,87,37,33,76,98,89,19,69,9,62,21,38,49,54,81,0,26,79,36,57,18,4,40,31,80,24,64,77,97,70,6,73,23,20,47,45,51,74,25,95,96,58,92,94,11,39,63,65,99,48,83,29,34,44,75,55,17,14,56,8,82,59,52,46,90,5,41,60,67,16,1,15,61,71,66,72,30,28,3,43,27,78,10,86,7,50,35,84,12,93,68)

Input$Row = 1:nrow(Input)
Input$Board = ceiling(Input$Row / (Board_Size + 1))
Input = Input[!is.na(V1),]

Result = NULL
Remaining_Boards = unique(Input$Board)

for(nDraw in Draws){
  #print(nDraw)
  Input$V1[Input$V1 == nDraw] = -1
  Input$V2[Input$V2 == nDraw] = -1
  Input$V3[Input$V3 == nDraw] = -1
  Input$V4[Input$V4 == nDraw] = -1
  Input$V5[Input$V5 == nDraw] = -1
  
  if(length(Remaining_Boards) > 1){
    for (nBoard in Remaining_Boards){
      TempB = Input[Board == nBoard]
      TempB = as.matrix(TempB[,list(V1, V2, V3, V4, V5)])
      if (any(rowSums(TempB) == -Board_Size) | any(colSums(TempB) == -Board_Size)){
        Remaining_Boards = Remaining_Boards[Remaining_Boards != nBoard]
      }
    }
  }
  
  if(length(Remaining_Boards) == 1){
    for (nBoard in Remaining_Boards[1]){
      TempB = Input[Board == nBoard]
      TempB = as.matrix(TempB[,list(V1, V2, V3, V4, V5)])
      if (any(rowSums(TempB) == -Board_Size) | any(colSums(TempB) == -Board_Size)){
        Result = sum(TempB[which(TempB != -1)], na.rm = T) * nDraw
        break
      }
    }
  }
  
  if(!is.null(Result)){
    break
  }
  
}

Result



