#install.packages('magrittr')
#install.packages("magick", verbose=TRUE)
#install.packages("openxlsx")


#library('tidyverse') #tibble vs table
library('magick')
library('rgl') 
library('magrittr')
library("openxlsx")

setwd("D:/R stuff/smaig-v02") #My AIG files
source("SMAIGpkg.R")  #treating this like a package

SMAIGtable <- read.xlsx("SMMRT_Final.xlsx")  # read first sheet of your deck of stacks
#head(SMAIGtable[, 1:5])

View(SMAIGtable)
##  CHECK to see if you need to delete the first column and run the line below.
#SMAIGtable[1] <- NULL # deletes the column that write.xlsx inserts into the table.

# the table has coordinates for each cube in stacks 1-n; the last row (11th) is the rotation coordinates
# SMAIGTable <- SMAIGTable[-c(31:50), ] ## delete rows 
# attach(SMAIGTable); newSMAIGTable<- SMAIGTable[which(Stack_ID == 1), ]; detach(SMAIGTable)
#rm(SMAIG)

#-----------------------------------------------------------------------------------
#FUNCTION calls!!  
#-----------------------------------------------------------------------------------
##Build the Stack!!
#-----------------------------------------------------------------------------------
# This "randomly" repopulates the cubeCoord matrix variable
  cubeCoord = buildStack() 

##OR repopulate the cubeCoord matrix with SMAIGTable data for the stackID passed
  cubeCoord <- tableStack(stackID =1) # if stackID==0, choose first stack in SMAIGtable
#-----------------------------------------------------------------------------------
## DISPAY the stack in a window - careful, RStudio may place this window in a different monitor if available.
  displayStack(cMarker = F) #cMarker: blue for first cube and red for last cube
  displayStack(cMarker = T)
  #OR
  displayStack(newScreen = TRUE) # new device screen, this allows you to compare stacks
  displayRainbow()
  
# move focus between two or more device screens; up or down
  changeFocus() # default is upFocus = FALSE  
  changeFocus(upFocus = TRUE)

#build mirror in the same screen
  twinStack(cMarker = TRUE) #

#build the same model in two screens, rotate the 2nd
  rotateTest() 

#Check disparity between the two screens
  evalTest()
   
#build mirror in cleared screen  (saveStack must be TRUE if you want to save the mirror)
  cubeCoord = mirrorStack(cMarker = F, saveStack = T)
# arguments:
#saveStack=T saves the mirror to cubeCoord; you can then store it and save it to table/xls
#cMarker=T blue for first cube and red for last cube

#rgl.viewpoint(); 
# either manipulate the stack with mouse or use the mirrorStack function to change rotation
# use store3d function to save the changed coordinates to cubeCoord
  cubeCoord <- tableStack(stackID=2)
    cubeCoord = store3d()
    displayStack(cMarker = T)
  cubeCoord
# appends cubeCoord (the current stack) to SMAIGTable (which you can save to excel)
  SMAIGtable <- saveStack() 
  
## write the table to an Excel file  
  write.xlsx(SMAIGtable, file = "SMAIG.xlsx")
  
# save picture as a png to your working directory. WARNING: overwrites
  setwd("D:/R stuff/smaig-v02/smaigPics") # Change to reflect your working directory
  i = 99 # should use the stackID in the SMAIGtable to match pictures' names to data

    snapName <- paste0("smaig ", formatC(i), ".png") # create a variable with the name of the new picture
  snapName #display name in RStudio console
  rgl.snapshot(snapName)
  ##

  setwd("D:/R stuff/AIG") # Change to reflect your working directory

  #-----------------------------------------------------------------------------------
  #-----------------------------------------------------------------------------------
  ## Save pictures of all stacks in current SMAIGtable
  # save picture as a png to your working directory. WARNING: overwrites
  setwd("D:/R stuff/SMAIG_occlusion/smaigGray") # Change to reflect your working directory
  stackCount  = SMAIGtable[nrow(SMAIGtable) ,1]
  for (i in 1:stackCount) {
    cubeCoord <- tableStack(stackID = i) 
    displayRainbow()
    #displayRainbow2(stackCount = i) #stackCount sets each stack to one solid color
    displayStack(cMarker = F) 
    snapName <- paste0("Gray_", formatC(i), ".png") # create a variable with the name of the new picture
    
    if (i < 10) {
    snapName <- paste0("Gray_0", formatC(i), ".png") # create a variable with the name of the new picture
    }
        snapName #display name in RStudio console
    rgl.snapshot(snapName)
  }

#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
##movie3d(spin3d(axis=c(20,-10,-60), rpm=4), duration=10) 
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
