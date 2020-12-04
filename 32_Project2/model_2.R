library(tidyverse)
library(R2OpenBUGS)


#prepare the data for input into OpenBUGS
y <- as.matrix(read.table("ExtraDataForR_Report2(1)/DataProteins413.csv", sep = ","))
n <- 9
p <- 413
data <- list ("y", "n", "p")

#initialization of variables
inits <- list(
  list(mu = rep(0, 413), tau = rep(1, 413), a = 1, b = 1, pred.pos = rep(1,413), pred.mix = rep(1,413)),
  list(mu = rep(6, 413), tau = rep(7, 413), a = 7, b = 7, pred.pos = rep(1,413), pred.mix = rep(1,413))
)

#set the WINE working directory and the directory to OpenBUGS - change the OpenBUGS.exe location as necessary
WINE="/usr/local/bin/wine"
WINEPATH="/usr/local/bin/winepath"
OpenBUGS.pgm="/Users/salvadorgarcia/.wine/drive_c/Program Files/OpenBUGS/OpenBUGS323/OpenBUGS.exe"

#these are the parameters to save
parameters = c("mu", "tau", "a", "b", "pred.pos", "pred.mix", "M.pos", "M.mixed")

#run the model
schools.sim <- bugs(data, inits, 
                    model.file = "model_2.txt",
                    parameters=parameters,
                    n.chains = 2, 
                    n.iter = 3000, 
                    n.thin = 10,
                    OpenBUGS.pgm=OpenBUGS.pgm,
                    WINE=WINE, 
                    WINEPATH=WINEPATH,
                    useWINE=T, debug = FALSE)

hist(c(apply(schools.sim$sims.matrix %>% data.frame() %>% dplyr::select(contains("M.pos")), 2, mean)))
hist(c(apply(schools.sim$sims.matrix %>% data.frame() %>% dplyr::select(contains("M.mixed")), 2, mean)))




