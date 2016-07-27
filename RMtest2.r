rm(list=ls())

## capture all the output to a file.
setwd("/work/project/LOGS/RMsavetest/")
file.create("RMtest2.Rout")
zz <- file("/work/project/LOGS/RMsavetest/RMtest2.Rout", open = "wt")
sink(zz)

t_0 = date()

# ===========================================================
#  RMtest2.R
# ===========================================================
# creates "inventory" data-frame
#
# Variables:
# ----------------
# - state
# - yr
# - filepath
# - Truth 
# - TraningSet
# 
#
#
#


library(ReadMe)

#  LOAD Inventory & TrainingSet(s)
# ==========================================================

library(ReadMe)

trnset0 <- "/work/project/control/oldcontrol/_control.csv"

ctrl <- read.csv(trnset0, header=TRUE)
inventory <- read.csv("/work/project/LOGS/inventory.csv", header=TRUE)
indices <- c('state', 'yr')

finaldir <- "/work/project/final"

#  READEM: takes vector of states (and years)
#  if given years: runs state/yr stratified readme calls
#  if no years: runs state-stratified readme calls
# ==========================================================

readem <- function (state=c(), yr=c(), boot=FALSE) {
  noyrs <- data.frame(state=c(),yr=c())
  
  for (i in 1:length(state)) {
    setwd( file.path(finaldir,state[i]) )
    
    # if state & yr specified
    if ( length(yr)>=1 ) {
      for (j in 1:length(yr)) {
        if ( !dir.exists( file.path(finaldir,state[i],yr[j] )) ) {
          noyrs <- rbind(noyrs,c(state[i],yr[j]))
        } else {
          
          t_styr_0 <- date()
          
          setwd( file.path(finaldir,state[i],yr[j]) )
          styr.inv <- inventory[inventory$state==state[i] & inventory$yr==yr[j],
                                !(names(inventory) %in% indices)]
          styr.inv$trainingset <- 0
          styr.inv <- rbind(ctrl, styr.inv)
          styr <- paste(state[i],yr[j], sep="")
          
          setwd('/work/project/LOGS/RMsavetest')
          write.csv(styr.inv, file=paste(styr,"_control.csv", sep=""), row.names=FALSE, quote=FALSE)
          
          # underCA2015overall.results
          assign(paste("under",styr,"overall.results", sep=""),
                 undergrad(control=paste( styr,"_control.csv", sep=""), sep=",") )
          
          # undergradCA2015overall.preprocess
          assign(paste("undergrad",styr,"overall.preprocess", sep=""),
                 preprocess(get( paste("under",styr,"overall.results", sep="")) ) )
          # readmeCA2015overall.results
          assign(paste("readme",styr,"overall.results", sep=""),
                 readme( get( paste("undergrad",styr,"overall.preprocess", sep="")),
                         boot.se=boot) )
          
          # save: CA2015overall.Rdata
          save(list= c(paste("readme",styr,"overall.results", sep=""), "noyrs"),
               file= paste(styr,"overall.Rdata",sep="") )
          
          t_styr_f <- date()
          
          print("--------------------------------------------")
          print(paste("RM state (",styr,")"," start: ", t_styr_0, sep=""))
          print(paste("RM state (",styr,")","   end: ", t_styr_f, sep=""))
          print("-------------------------------------")
          print(paste("FILE SAVED: ",getwd(),"/",styr,"overall.Rdata", sep=""))
          print("--------------------------------------------")
        }
      }
      
      # if only states specified  
    } else {
      st.inv <- rbind(ctrl)   
      yr  <- list.files()
      
      for (j in 1:length(yr)) {
        setwd( file.path(finaldir,state[i],yr[j]) )
        styr.inv <- inventory[inventory$state==state[i] & inventory$yr==yr[j],
                              !(names(inventory) %in% indices)]
        styr.inv$trainingset <- 0
        # styr.inv[is.na(styr.inv)] <- ""
        st.inv <- rbind(st.inv, styr.inv)
      }
      t_st_0 <- date()
      
      setwd('/work/project/LOGS/RMsavetest')
      write.csv(st.inv, file=paste(state[i],"_control.csv", sep=""), row.names=FALSE, quote=FALSE )
      
      # underCAoverall.results
      assign(paste("under",state[i],"overall.results", sep=""),
             undergrad(control=paste(state[i],"_control.csv", sep=""), sep=",") )
      
      # undergradCAoverall.preprocess
      assign(paste("undergrad",state[i],"overall.preprocess", sep=""),
             preprocess(get( paste("under",state[i],"overall.results", sep="")) ) )
      # readmeCAoverall.results
      assign(paste("readme",state[i],"overall.results", sep=""),
             readme( get( paste("undergrad",state[i],"overall.preprocess", sep="")),
                     boot.se=boot) )
      
      # save: CAoverall.Rdata
      save(list= c(paste("readme",state[i],"overall.results", sep=""), "noyrs"),
           file= paste(state[i],"overall.Rdata",sep="") )
      
      t_st_f <- date()
      
      print("--------------------------------------------")
      print(paste("RM state (",st,")"," start: ", t_st_0, sep=""))
      print(paste("RM state (",st,")","   end: ", t_st_f, sep=""))
      print("-------------------------------------")
      print(paste("FILE SAVED: ",getwd(),"/",st,"overall.Rdata", sep=""))
      print("--------------------------------------------")
    }
  }
}

# Readem Runs
# ==========================================================


#  Testing SD/2009 (n=49) sans Bootstrap
# ================================

setwd("/work/project/final")

st <- c('SD')
yrs <- c('2009')

try( readem(state=st, yr=yrs, boot=FALSE) )


#  Testing SD/2009 (n=49) w/ Bootstrap
# ================================

setwd("/work/project/final")

st <- c('SD')
yrs <- c('2009')

try( readem(state=st, yr=yrs, boot=TRUE) )


# SAVE AND LOG TIMES
# ================================================
setwd("/work/project/LOGS/RMsavetest")

t_f=date()

print("----------------------------------")
print(paste("RMtest2.r started:", t_0))
print(paste("RMtest2.r completed:", t_f))
print("----------------------------------")

sink()

q()

