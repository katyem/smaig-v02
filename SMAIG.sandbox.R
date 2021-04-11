#install.packages('magrittr')
#install.packages("magick", verbose=TRUE)
#install.packages("openxlsx")


#library('tidyverse') #tibble vs table
library('magick')
library('rgl') 
library('magrittr')
library("openxlsx")
getwd()
setwd("D:/R stuff/smaig-v02") #My AIG files
source("SMAIGpkg.R")  #treating this like a package

SMAIGtable <- read.xlsx("SMAIG.test.xlsx")  # read first sheet of your deck of stacks
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
  cubeCoord <- tableStack(stackID =18) # if stackID==0, choose first stack in SMAIGtable
#-----------------------------------------------------------------------------------
## DISPAY the stack in a window - careful, RStudio may place this window in a different monitor if available.
  displayStack(cMarker = F) #cMarker: blue for first cube and red for last cube
  displayStack(cMarker = T)
  #OR
  displayStack(newScreen = TRUE, cMarker = T) # new device screen, this allows you to compare stacks
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
 # SMAIGtable <- read.xlsx("SMMRT_Final.xlsx")  # read first sheet of your deck of stacks
  SMAIGtable <- read.xlsx("SMAIG.test.xlsx")  # read first sheet of your deck of stacks
  
  cubeCoord <- tableStack(stackID=1)
  displayStack(newScreen = TRUE, cMarker = T)
    
  cubeCoord <- tableStack(stackID=1)
  displayStack(newScreen = TRUE, cMarker = T, rglAxis = T)
  spheres3d(0, 0, 0, radius = 4.5, col=rgb(1,1,1), alpha=0.3)
  axes3d()
  grid3d(c("x", "y+", "z"))
  #title3d('main', 'sub', 'xlab', 'ylab', 'zlab')
  movie3d(spin3d(axis=c(120,-120,-120), rpm=5), duration=5, movie = "SM_3d_Spin_v5", dir = "D:/R stuff/smaig-v02/images", type = "gif") 
  
  displayStack(newScreen = TRUE, cMarker = T)

  cubeCoord = store3d()
  displayStack(newScreen = TRUE, cMarker = T)
  cubeCoord
  rgl_add_axes(x, y, z, show.bbox = TRUE)
# appends cubeCoord (the current stack) to SMAIGTable (which you can save to excel)
  SMAIGtable <- saveStack() 
  
## write the table to an Excel file  
  write.xlsx(SMAIGtable, file = "SMMRT_Rotations.xlsx")
  
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
movie3d(spin3d(axis=c(90,-90,-90), rpm=2), duration=10, movie = "3d_Spin", dir = "D:/R stuff/smaig-v02/images") 
  

#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
## Setting up axes with markers in the rgl image: http://www.sthda.com/english/wiki/a-complete-guide-to-3d-visualization-device-system-in-r-r-software-and-data-visualization#rgl_add_axes-a-custom-function-to-add-x-y-and-z-axes
#-----------------------------------------------------------------------------------
  displayStack(newScreen = T, cMarker = T, rglAxis = T)
  x <- 3
  y <- 3
  z <- 3
    rgl_add_axes(4,4,4)
    axes3d() 
    # x, y, z : numeric vectors corresponding to
  #  the coordinates of points
  # axis.col : axis colors
  # xlab, ylab, zlab: axis labels
  # show.plane : add axis planes
  # show.bbox : add the bounding box decoration
  # bbox.col: the bounding box colors. The first color is the
  # the background color; the second color is the color of tick marks
rgl_add_axes <- function(x, y, z, axis.col = "grey",
                           xlab = "x", ylab="y", zlab="z", show.plane = FALSE, 
                           show.bbox = FALSE, bbox.col = c("#333377","black"))
  { 
    
    lim <- function(x){c(-max(abs(x)), max(abs(x))) * 1.1}
    # Add axes
    xlim <- lim(x); ylim <- lim(y); zlim <- lim(z)
    rgl.lines(xlim, c(0, 0), c(0, 0), color = axis.col)
    rgl.lines(c(0, 0), ylim, c(0, 0), color = axis.col)
    rgl.lines(c(0, 0), c(0, 0), zlim, color = axis.col)
    
    # Add a point at the end of each axes to specify the direction
    axes <- rbind(c(xlim[2], 0, 0), c(0, ylim[2], 0), 
                  c(0, 0, zlim[2]))
    rgl.points(axes, color = axis.col, size = 3)
    
    # Add axis labels
    rgl.texts(axes, text = c(xlab, ylab, zlab), color = axis.col,
              adj = c(0.5, -0.8), size = 2)
    
    # Add plane
    if(show.plane) {
      xlim <- xlim/1.1; zlim <- zlim /1.1
      rgl.quads( x = rep(xlim, each = 2), y = c(0, 0, 0, 0),
      z = c(zlim[1], zlim[2], zlim[2], zlim[1]))
    }
    # Add bounding box decoration
    if(show.bbox){
      rgl.bbox(color=c(bbox.col[1],bbox.col[2]), alpha = 0.5, 
               emission=bbox.col[1], specular=bbox.col[1], shininess=5, 
               xlen = 3, ylen = 3, zlen = 3) 
    }
  }
