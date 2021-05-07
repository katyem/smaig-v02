library('magick')
library('rgl') 
library('magrittr')
library("openxlsx")

setwd("D:/R stuff/smaig-v02") #My AIG files
source("SMAIGpkg.R")  #treating this like a package

SMAIGtable <- read.xlsx("SMMRT_Final.xlsx")  # read first sheet of your deck of stacks

# R doesn't have a trace function to add the diagonal of the matrix; I found this which seems to work: https://rpubs.com/aaronsc32/matrix-trace
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

A <- diag(1,3,3) # diagonals <- 1; sets the starting point of the matrix in 3d

for (i in 1:(NROW(SMAIGtable)/(stackSize+1))) {  

  cubeCoord <- tableStack(stackID = i)
  # cubeCoord <- tableStack(stackID = i)
  #print(cubeCoord[11,])
  #displayStack(newScreen <- TRUE, cMarker <- T)
  B <- diag(1,3,3) # diagonals <- 1; sets the starting point of the matrix in 3d
  
  # Use the Euler angles stored in the last row (stackSize+1) of cubeCord
  # rotate3d translates the Euler angles to coordinates in the 3x3 matrix
  # rotate3d(obj, angle, x, y, z, matrix, ...)
  B <- rotate3d(B, pi*(cubeCoord[stackSize+1,1]/180), 1,0,0) # Rotate about model's x axis
  B <- rotate3d(B, pi*(cubeCoord[stackSize+1,2]/180), 0,1,0) # Rotate about model's y axis
  B <- rotate3d(B, pi*(cubeCoord[stackSize+1,3]/180), 0,0,1) # Rotate about model's z axis
  
  SMAIGtable[i*(stackSize+1),5] <- acos((trace(t(B)%*%A)-1)/2)*(360/2/pi) # difference between two matrices;%*% is a matrices multiplication 
  #t() inverts matrice
}


##### STOP ######################################################################
## Other stuff
# Changed Octive alogrithm to R: https://math.stackexchange.com/questions/2113634/comparing-two-rotation-matrices
A <- matrix( c(−0.956395958,−0.292073218,0.000084963,0.29207323,−0.956395931,0.000227268,0.00001488,0.000242173,0.999999971),3,3)
B <- matrix( c(−0.956227882,−0.292623029,−0.000021887,0.29262303,−0.956227882,−0.000024473,−0.000013768,−0.000029806,0.999999999),3,3)

trace(B)
acos((trace(t(A)%*%B)-1)/2)*(360/2/pi) # difference between two matrices;%*% is a matrices multiplication 
t(A)  # invert matrix

