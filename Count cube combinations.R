legRange = matrix(c(2,5,2,5,2,5,1,4))
legRange <- matrix(legRange,nrow=4,ncol=2,byrow=TRUE)
# calculate the combination within range parameters legRange and compare to each reversed stack in the stackMatrix

stackMatrix <- matrix(0, nrow=40, ncol=4)  #over esimate the rows
matchCount <- 0

  for (leg1 in legRange[1,1]:legRange[1,2]) { #range of first leg
    #print(leg1)
    for (leg2 in legRange[2,1]:legRange[2,2]) { #range of second leg
      #print(leg2)
      for (leg3 in legRange[3,1]:legRange[3,2]) { #range of third leg
        #print(leg3)
        for (leg4 in legRange[4,1]:legRange[4,2]) { #range of fourth leg
            legCheck <- c(leg1,leg2,leg3,leg4)
            match <- FALSE
          if (sum(legCheck) == 10) {
            if (matchCount > 1) {
              for (i in 1:matchCount) { # look through stackMatrix for a match
                #check for inverse
                if (legCheck[4]+1 == stackMatrix[i,1] && legCheck[3] == stackMatrix[i,2] && legCheck[2] == stackMatrix[i,3] && legCheck[1]-1 == stackMatrix[i,4]) {
                  match <- TRUE
                  break
                }
              }
            }
          } else {
            match <- TRUE  # not saving it if it's not equal to 10
          }
          if (match == F) {
            matchCount<- matchCount+1
            stackMatrix[matchCount,1:4] <- legCheck
            print(legCheck)
          }
        }
      }
    }
  }
print("Number of unique combinations:")
print(matchCount)
