options(scipen = 9999)  #disable scientific notation

#libraries
library(data.table)
library(igraph)

#Read in the data
Input = data.table(readLines('C:/Users/noble/Desktop/AOC 2021/AOC_2021_D15.txt'))
Input$V1 = as.character(Input$V1)
x = strsplit(Input$V1, '')

Move_Val = function(Cur_Spot){
  #Determine all moves from the current spot
  Moves = data.table(rbind(c(Cur_Spot[1] - 1, Cur_Spot[2]), 
                            c(Cur_Spot[1] + 1, Cur_Spot[2]), 
                            c(Cur_Spot[1], Cur_Spot[2] - 1), 
                            c(Cur_Spot[1], Cur_Spot[2] + 1)))
  setnames(Moves, c('Row', 'Col'))
  
  #Remove invalid spots and merge the ID onto the found moves
  Moves = Moves[!(Moves$Row %in% c(0, nrow(Mat) + 1)) & !(Moves$Col %in% c(0, ncol(Mat) + 1)),]
  Moves = merge(Moves, All_Steps, by = c('Row', 'Col'), all.x = T)
  
  #Add the 'risk' of moving from the current spot to each adjacent spot
  Paths = data.table(cbind(Cur_Spot[3], Moves$ID, apply(Moves, 1, function(x) Mat[x[1], x[2]])))
  setnames(Paths, c('Start', 'End', 'Val'))
  
  Paths
}


#Part 1
#Create a matrix of the the data
Mat = matrix(0, nrow = nrow(Input), ncol = nchar(Input$V1[1]))
for(iRow in 1:nrow(Input)){
  Mat[iRow,] = as.numeric(x[[iRow]])
}

#create an ID list of all points
All_Steps = expand.grid(1:nrow(Mat), 1:nrow(Mat))
setnames(All_Steps, c('Row', 'Col'))
All_Steps$ID = 1:nrow(All_Steps)

#Call function to find all mappings
Result = do.call(rbind, apply(All_Steps, 1, Move_Val))

#Create a graph object and add the weights
Result_Graph = graph_from_edgelist(as.matrix(Result[,1:2]))
E(Result_Graph)$weight = Result$Val

#Result
distances(graph = Result_Graph, 1, to = max(All_Steps$ID), mode = "out")



#Part 2
#Create a matrix of the the data
Mod_Mat = matrix(0, nrow = nrow(Mat) * 5, ncol = ncol(Mat) * 5)
for(Row_Shift in 0:4){
  for(iRow in 1:nrow(Mat)){
    Mod_Mat[iRow + (Row_Shift * nrow(Mat)),] = c(Mat[iRow,] + Row_Shift,
                                                 Mat[iRow,] + 1 + Row_Shift, 
                                                 Mat[iRow,] + 2 + Row_Shift, 
                                                 Mat[iRow,] + 3 + Row_Shift, 
                                                 Mat[iRow,] + 4 + Row_Shift)
  }
}
Mod_Mat = ifelse(Mod_Mat < 10, Mod_Mat, Mod_Mat - 9)
Mat = Mod_Mat

#create an ID list of all points
All_Steps = expand.grid(1:nrow(Mat), 1:nrow(Mat))
setnames(All_Steps, c('Row', 'Col'))
All_Steps$ID = 1:nrow(All_Steps)

#Call function to find all mappings
Result = do.call(rbind, apply(All_Steps, 1, Move_Val))

#Create a graph object and add the weights
Result_Graph = graph_from_edgelist(as.matrix(Result[,1:2]))
E(Result_Graph)$weight = Result$Val

#Result
distances(graph = Result_Graph, 1, to = max(All_Steps$ID), mode = "out")






