#Load the OpenBUGS Package - make sure XQuartz is running
library(tidyverse)
library(R2OpenBUGS)

#define the model
nummodel <- function(){
  theta     ~ dbeta(9.2, 13.8)         
  y         ~ dbin(theta,n)                
  odds  <- theta/(1-theta)              
  p.high   <- step(theta - 0.5)  
}

# write the model code out to a file
write.model(nummodel, "nummodel.txt")
model.file1 = paste(getwd(),"nummodel.txt", sep="/")
## and let's take a look:
file.show("nummodel.txt")

#prepare the data for input into OpenBUGS
y <- 15
n <- 20
data <- list ("y", "n")

#initialization of variables
inits <- function(){
  list(theta = 0.1, theta = 0.9)}

#set the WINE working directory and the directory to OpenBUGS - change the OpenBUGS.exe location as necessary
WINE="/usr/local/bin/wine"
WINEPATH="/usr/local/bin/winepath"
OpenBUGS.pgm="/Users/salvadorgarcia/.wine/drive_c/Program Files/OpenBUGS/OpenBUGS323/OpenBUGS.exe"

#these are the parameters to save
parameters = c("theta", "odds", "p.high")

#run the model
schools.sim <- bugs(data, inits, 
                    model.file = model.file1,
                    parameters=parameters,
                    n.chains = 3, 
                    n.iter = 3000, 
                    OpenBUGS.pgm=OpenBUGS.pgm,
                    WINE=WINE, 
                    WINEPATH=WINEPATH,
                    useWINE=T)
schools.sim$summary
  


