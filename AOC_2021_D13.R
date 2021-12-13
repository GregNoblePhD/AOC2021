options(scipen = 9999)  #disable scientific notation

library(data.table)

Input = data.table(readLines('C:/Users/noble/Desktop/AOC 2021/AOC_2021_D13.txt'))

Dots = Input[1:(which(Input$V1 == '')-1),]
Instr = Input[(which(Input$V1 == '')+1):nrow(Input),]

Dots = strsplit(Dots$V1, ',')
Dots = data.table(cbind('X' = as.numeric(sapply(Dots, "[[", 1)), 
                         'Y' = as.numeric(sapply(Dots, "[[", 2))))

Instr$V1 = gsub('fold along ', '', Instr$V1)
Instr = strsplit(Instr$V1, '=')
Instr = data.table(cbind('Axis' = sapply(Instr, "[[", 1), 
                         'Val' = as.numeric(sapply(Instr, "[[", 2))))
Instr$Val = as.numeric(Instr$Val)

Mat = matrix(0, nrow = max(Dots$Y) + 1, ncol = max(Dots$X) + 1)

for (iDot in 1:nrow(Dots)){
  Mat[Dots$Y[iDot] + 1, Dots$X[iDot] + 1] = 1
}

Fold_Mat = function(Mat, Rounds){
  for (iInstr in 1:Rounds){
    if(Instr$Axis[iInstr] == 'y'){
      Mat1 = Mat[1:(Instr$Val[iInstr]),]
      Mat2 = Mat[nrow(Mat):(Instr$Val[iInstr]+2),]
      Mat = Mat1 + Mat2
    }
    if(Instr$Axis[iInstr] == 'x'){
      Mat1 = Mat[,1:(Instr$Val[iInstr])]
      Mat2 = Mat[,ncol(Mat):(Instr$Val[iInstr]+2)]
      Mat = Mat1 + Mat2
    }
  }
  return(Mat)
}

#Part 1
Fold1_Mat = Fold_Mat(Mat, 1)
length(which(Fold1_Mat != 0))

#Part 2
Fold_All_Mat = Fold_Mat(Mat, nrow(Instr))
Fold_All_Mat[Fold_All_Mat > 1] = 1
View(Fold_All_Mat)


