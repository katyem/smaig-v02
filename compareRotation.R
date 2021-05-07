library('magick')
library('rgl') 
library('magrittr')
library("openxlsx")

setwd("D:/R stuff/smaig-v02") #My AIG files
source("SMAIGpkg.R")  #treating this like a package

SMAIGtable <- read.xlsx("SMAIG.test.Rotations.xlsx")  # read first sheet of your deck of stacks

# R doesn't have a trace function to multiply the diagonal of the matrix; I found this which seems to work: https://rpubs.com/aaronsc32/matrix-trace
trace <- function(Amatrix) {  
  n <- dim(Amatrix)[1] # get dimension of matrix
  tr <- 0 # initialize trace value
  # Loop over the diagonal elements of the supplied matrix and add the element to tr
  for (k in 1:n) {
    l <- Amatrix[k,k]
    tr <- tr + l
  }
  return(tr[[1]])
}

# Ready player one!
stackSize <- 10

##cubeCoord <- tableStack(stackID <-1) # Use if -> First stack is zero rotation
#print(cubeCoord[11,])
#displayStack(newScreen <- TRUE, cMarker <- T)


A <- diag(1,3,3) # diagonals <- 1; sets the starting point of the matrix in 3d

# WAIT!!!  You don't need to reset A if you want to compare to zero rotation
# Use the Eular angles stored in the last row (stackSize+1) of cubeCord
A <- rotate3d(A, pi*(cubeCoord[stackSize+1,1]/180), 1,0,0) # Rotate about model's x axis
A <- rotate3d(A, pi*(cubeCoord[stackSize+1,2]/180), 0,1,0) # Rotate about model's y axis
A <- rotate3d(A, pi*(cubeCoord[stackSize+1,3]/180), 0,0,1) # Rotate about model's z axis



for (i in 2:(NROW(SMAIGtable)/(stackSize+1))) {  

  cubeCoord <- tableStack(stackID = i)
  #print(cubeCoord[11,])
  #displayStack(newScreen <- TRUE, cMarker <- T)
  B <- diag(1,3,3) # diagonals <- 1; sets the starting point of the matrix in 3d
  
  # Use the Eular angles stored in the last row (stackSize+1) of cubeCord
  B <- rotate3d(B, pi*(cubeCoord[stackSize+1,1]/180), 1,0,0) # Rotate about model's x axis
  B <- rotate3d(B, pi*(cubeCoord[stackSize+1,2]/180), 0,1,0) # Rotate about model's y axis
  B <- rotate3d(B, pi*(cubeCoord[stackSize+1,3]/180), 0,0,1) # Rotate about model's z axis
  
  SMAIGtable[i*(stackSize+1),5] <- acos((trace(t(B)%*%A)-1)/2)*(360/2/pi) # difference between two matrices;%*% is a matrices multiplication 
  #t() inverts matrice
}


##### STOP ######################################################################

# Compare two stacks from the same SMAIGtable
SMAIGtable <- read.xlsx("SMMRT_Final.xlsx")  # read first sheet of your deck of stacks

list = matrix(c(2, 4, 1, 3, 1, 3, 2, 4, 1, 3, 2, 
                 4, 2, 4, 1, 3, 1, 2, 3, 4, 1, 2, 
                 3, 4), nrow = 24, ncol = 1)
list = cbind(list, c(15, 27, 9, 21, 10, 22, 16, 28, 
                     23, 19, 13, 25, 14, 26, 8, 20, 
                     5, 11, 17, 23, 6, 12, 18, 24))

for (i in 1:NROW(list)) {  
  
  cubeCoord <- tableStack(stackID = list[i,1])
  A <- diag(1,3,3) # diagonals <- 1; sets the starting point of the matrix in 3d
  A <- rotate3d(A, pi*(cubeCoord[stackSize+1,1]/180), 1,0,0) # Rotate about model's x axis
  A <- rotate3d(A, pi*(cubeCoord[stackSize+1,2]/180), 0,1,0) # Rotate about model's y axis
  A <- rotate3d(A, pi*(cubeCoord[stackSize+1,3]/180), 0,0,1) # Rotate about model's z axis
  
  cubeCoord <- tableStack(stackID = list[i,2])
  B <- diag(1,3,3) # diagonals <- 1; sets the starting point of the matrix in 3d
  
  # Use the Eular angles stored in the last row (stackSize+1) of cubeCord
  B <- rotate3d(B, pi*(cubeCoord[stackSize+1,1]/180), 1,0,0) # Rotate about model's x axis
  B <- rotate3d(B, pi*(cubeCoord[stackSize+1,2]/180), 0,1,0) # Rotate about model's y axis
  B <- rotate3d(B, pi*(cubeCoord[stackSize+1,3]/180), 0,0,1) # Rotate about model's z axis
  
  #print(paste("Pair ", i, ": ", list1[i], "/", list2[i], round(acos((trace(t(B)%*%A)-1)/2)*(360/2/pi),4))) # difference between two matrices;%*% is a matrices multiplication 
  print(paste(round(acos((trace(t(B)%*%A)-1)/2)*(360/2/pi),4))) # difference between two matrices;%*% is a matrices multiplication 
  #t() inverts matrice
}




## Other stuff
# Changed Octive alogrithm to R: https://math.stackexchange.com/questions/2113634/comparing-two-rotation-matrices

trace(B)
acos((trace(t(A)%*%B)-1)/2)*(360/2/pi) # difference between two matrices;%*% is a matrices multiplication 
t(A)  # invert matrix

