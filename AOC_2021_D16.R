options(scipen = 9999)  #disable scientific notation

#Libraries
library(data.table)

#Read in data
Input = strsplit(readLines('C:/Users/noble/Desktop/AOC 2021/AOC_2021_D16.txt'), '')[[1]]

#Given from problem statement
Hex_to_Binary = data.table(rbind(c('0', '0000'), 
                                 c('1', '0001'),
                                 c('2', '0010'), 
                                 c('3', '0011'),
                                 c('4', '0100'), 
                                 c('5', '0101'),
                                 c('6', '0110'), 
                                 c('7', '0111'),
                                 c('8', '1000'), 
                                 c('9', '1001'),
                                 c('A', '1010'), 
                                 c('B', '1011'),
                                 c('C', '1100'), 
                                 c('D', '1101'),
                                 c('E', '1110'), 
                                 c('F', '1111')))
setnames(Hex_to_Binary, c('Hex', 'Binary'))

#A vector of the binary value bits
Binary_Val = as.integer(strsplit(paste(Hex_to_Binary$Binary[match(Input, Hex_to_Binary$Hex)], 
                                       collapse = ''), 
                                 '')[[1]]
                        )

# strtoi can't handle really large values.
#Bit_to_Integer = function(Bin_Val) {
#  return(strtoi(paste(Bin_Val, collapse = ''), base = 2))
#}

#Funciton to convert binary bits to an integer
Bit_to_Integer <- function(Bin_Val) {sum(Bin_Val * 2^rev((seq_along(Bin_Val)-1)))}

#Function to solve the problem
Version_Sum = function(Bin_Val, Sum_of_Ver){
  
  #Pull the version and add to sum (Part 1)
  Version = Bit_to_Integer(Bin_Val[1:3])
  Bin_Val = Bin_Val[-(1:3)]
  Sum_of_Ver = Sum_of_Ver + Version
  
  #Pull the type to know what to do next
  Type = Bit_to_Integer(Bin_Val[1:3])
  Bin_Val = Bin_Val[-(1:3)]
  
  #Type 4 are literal values
  if(Type == 4){
    
    #loop through until Last_Ind = 0, indicating that it is the last one
    Last_Ind = 1
    Num_Val = NULL
    while(Last_Ind == 1){
      Cur_Lit_Group = Bin_Val[1:5]
      Num_Val = c(Num_Val, Cur_Lit_Group[2:5])
      Bin_Val = Bin_Val[-(1:5)]
      Last_Ind = Cur_Lit_Group[1]  
    }
    
    #convert to a int value
    Tot_Num_Val = Bit_to_Integer(Num_Val)
    
  } else { #else it is an operator packet
    #Determine the length type
    Num_Val = NULL
    Len_Type = Bin_Val[1]
    Bin_Val = Bin_Val[-(1)]
    
    #Length type of 0 indicates the total length of the subpackets
    if(Len_Type == 0){
      #Determine the subpacket length
      SP_Len = Bit_to_Integer(Bin_Val[1:15])
      Bin_Val = Bin_Val[-(1:15)]
      
      #while there are still bits in the subpacket, recursively call the function
      Sub_Packets = Bin_Val[1:SP_Len]
      while(length(Sub_Packets) > 0){
        x = Version_Sum(Sub_Packets, Sum_of_Ver)
        Sum_of_Ver = x$Sum_of_Ver
        Sub_Packets = x$Bin
        Num_Val = c(Num_Val, x$Tot_Num_Val)
      }
      Bin_Val = Bin_Val[-(1:SP_Len)]
      
    #Length type of 1 indicates the number of supbackets
    } else {
      #Determine the number of subpackets
      SP_Num = Bit_to_Integer(Bin_Val[1:11])
      Bin_Val = Bin_Val[-(1:11)]
      
      #while there are still unprocessed subpackets, recursively call the function
      for(iSP in 1:SP_Num){
        x = Version_Sum(Bin_Val, Sum_of_Ver)
        Sum_of_Ver = x$Sum_of_Ver
        Bin_Val = x$Bin
        Num_Val = c(Num_Val, x$Tot_Num_Val)
      }
    }
    
  }
  
  #Determine the value if not Type == 4 (part 2)
  if(Type == 0){Tot_Num_Val = sum(Num_Val)}
  if(Type == 1){Tot_Num_Val = prod(Num_Val)}
  if(Type == 2){Tot_Num_Val = min(Num_Val)}
  if(Type == 3){Tot_Num_Val = max(Num_Val)}
  if(Type == 5){Tot_Num_Val = ifelse(Num_Val[1] > Num_Val[2], 1, 0)}
  if(Type == 6){Tot_Num_Val = ifelse(Num_Val[1] < Num_Val[2], 1, 0)}
  if(Type == 7){Tot_Num_Val = ifelse(Num_Val[1] == Num_Val[2], 1, 0)}
  
  return(list(Bin = Bin_Val, Version = Version, Sum_of_Ver = Sum_of_Ver, Tot_Num_Val = Tot_Num_Val))
}


#Part 1
Version_Sum(Binary_Val, 0)$Sum_of_Ver

#Part 2
Version_Sum(Binary_Val, 0)$Tot_Num_Val




