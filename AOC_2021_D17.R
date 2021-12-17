options(scipen = 9999)  #disable scientific notation

#libraries
library(data.table)

#Target Area
Targ_Min_X = 211
Targ_Max_X = 232
Targ_Min_Y = -124
Targ_Max_Y = -69

Min_Valid_X_Vel = ceiling((-1 + sqrt(1+8*Targ_Min_X))/2)

Y_Highest = 0       #Part 1 Result Tracking
Solution_Count = 0  #Part 2 Result Tracking

#Loop through all x,y velocity combos in a reasonable range
for(yStep in Targ_Min_Y:200){
  for(xStep in Min_Valid_X_Vel:Targ_Max_X){
    
    #Track variables
    Cur_Height = 0
    X_Vel = xStep 
    Y_Vel = yStep
    Cur_X = 0
    Cur_Y = 0
    
    #while Cur_X is stil below max X and Cur_Y above min Y
    while(Cur_X <= Targ_Max_X & Cur_Y >= Targ_Min_Y){
      
      #Step forward position
      Cur_X = Cur_X + X_Vel
      Cur_Y = Cur_Y + Y_Vel
      
      #Step forward velocity
      X_Vel = ifelse(X_Vel - 1 < 0, 0, X_Vel - 1)
      Y_Vel = Y_Vel - 1
      
      #Check for height improvement
      Cur_Height = max(Cur_Height, Cur_Y)
      
      #Check for a valid solution
      if(Cur_X >= Targ_Min_X & Cur_X <= Targ_Max_X & Cur_Y >= Targ_Min_Y & Cur_Y <= Targ_Max_Y){
        Y_Highest = max(Y_Highest, Cur_Height)
        Solution_Count = Solution_Count + 1
        
        #Result found, move to next set of velocities
        break
      }
    }
  }
}

#Part 1 
Y_Highest

#Part 2
Solution_Count
