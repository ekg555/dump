rm(list=ls())

## capture all the output to a file.
setwd("/work/project/LOGS")
file.create("RMtest.Rout")
zz <- file("/work/project/LOGS/RMtest.Rout", open = "wt")
sink(zz)

t_0 = date()

# ===========================================================
#  RMtest.R
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
          
          setwd('/work/project/LOGS')
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
      
      setwd('/work/project/LOGS')
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



#   By STATE/YR
# ================================

# SMALL: LA/2010 (n=540)

st <- c('LA')
yrs <- c('2010')

t1m_0 <- date()
try( readem(st,yrs, boot=TRUE) )
t1m_f <- date()
# -----------------------------------

# LARGE: NY/2006 (n=1169)

st <- c('NY')
yrs <- c('2006')

t1M_0 <- date()
try( readem(st,yrs, boot=TRUE) )
t1M_f <- date()


#  By STATE
# ==============================


# SMALL: PR (n=141)

st <- c('PR')

t2m_0 <- date()
try( readem(st, boot=TRUE) )
t2m_f <- date()

# -----------------------------------

# LARGE: DE (n=1303)

st <- c('DE')

t2M_0 <- date()
try (readem(st, boot=TRUE) )
t2M_f <- date()



# SAVE AND LOG TIMES
# ================================================
setwd("/work/project/LOGS")

t_f=date()

print(paste("RMtest.r started:", t_0))
print("--------------------------------------------")
print(paste("RM LA/2010 (n=540)  start:", t1m_0))
print(paste("RM LA/2010 (n=540)    end:", t1m_f))
print("-------------------------------------")
print(paste("RM NY/2006 (n=1169) start:", t1M_0))
print(paste("RM NY/2006 (n=1169)   end:", t1M_f))
print("-------------------------------------")
print(paste("RM PR (n=141) start:", t2m_0))
print(paste("RM PR (n=141)   end:", t2m_f))
print("-------------------------------------")
print(paste("RM DE (n=1303) start:", t2M_0))
print(paste("RM DE (n=1303)   end:", t2M_f))
print("--------------------------------------------")
print(paste("RMtest.r completed:", t_f))

sink()

q()

