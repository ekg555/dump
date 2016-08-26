rm(list=ls())

## capture all the output to a file.
setwd("/work/project/LOGS")
file.create("RM.Rout")
zz <- file("/work/project/LOGS/RM.Rout", open = "wt")
sink(zz)

library(ReadMe)

t_0 = date()

# ===========================================================
#  RM.R
#  UPDATED: 2016/08/26
# ===========================================================
#  DEFINES "readem [sic]" function that sequentially makes readme calls
#           at different-levels (state || state/yr)
#
#  REQUIRES (1) 'inventory.csv' file in a 'LOGS' folder
#                (a full inventory of all files that could be read by readme)
#           (2) some training set '_control.csv'
#                


#  LOAD Inventory & TrainingSet(s)
# ==========================================================

trnset0 <- "/work/project/control/oldcontrol/_control.csv"
finaldir <- "/work/project/final"

ctrl <- read.csv(trnset0, header=TRUE)
inventory <- read.csv("/work/project/LOGS/inventory.csv", header=TRUE)
indices <- c('state', 'yr')


#  READEM: takes vector of states (and years)
#    if given years: runs state/yr stratified readme calls
#    if no years: runs state-stratified readme calls
# ==========================================================

readem <- function (state=c(), yr=c(), boot=TRUE) {
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
          
          styr.inv <- inventory[inventory$state==state[i] & inventory$yr==yr[j],
                                !(names(inventory) %in% indices)]
	        styr.inv$trainingset <- 0
          styr.inv <- rbind(ctrl, styr.inv)
          
          # for file-label purposes...
          styr <- paste(state[i],yr[j], sep="")
          
          setwd('/work/project/LOGS')
          write.csv(styr.inv, file=paste(styr,"_control.csv", sep=""), row.names=FALSE, quote=FALSE)
          
          setwd( file.path(finaldir,state[i],yr[j]) )
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
        
          # save: CA2015overall.Rdata (in ST/YR & LOGS folder)
          print("--------------------------------------------")
          print(paste(styr," (n = ", nrow(styr.inv)-nrow(ctrl),")") )
          print("-------------------------------------")
          
          save(list= c(paste("readme",styr,"overall.results", sep=""), "noyrs"),
               file= paste(styr,"overall.Rdata",sep="") )
          print(paste("FILE SAVED: ",getwd(),"/",styr,"overall.Rdata", sep=""))
          
          setwd( file.path(finaldir,state[i],yr[j]) )
          save(list= c(paste("readme",styr,"overall.results", sep=""), "noyrs"),
               file= paste(styr,"overall.Rdata",sep="") )
          print(paste("FILE SAVED: ",getwd(),"/",styr,"overall.Rdata", sep=""))
          
          print("--------------------------------------------")
          t_styr_f <- date()
          print(paste("RM state (",styr,")"," start: ", t_styr_0, sep=""))
          print(paste("RM state (",styr,")","   end: ", t_styr_f, sep=""))
          print("--------------------------------------------")
         
        }
      }
      
    # if only states specified  
    } else {
      st.inv <- rbind(ctrl)   
      yr  <- list.files()
      
      for (j in 1:length(yr)) {
        
        setwd( file.path(finaldir,state[i],yr[j]) )
        st.inv <- inventory[inventory$state==state[i],
                              !(names(inventory) %in% indices)]
	      st.inv$trainingset <- 0
	      st.inv <- rbind(ctrl, st.inv)
	      
      }
      
      t_st_0 <- date()
      
      setwd('/work/project/LOGS')
      write.csv(st.inv, file=paste(state[i],"_control.csv", sep=""), row.names=FALSE, quote=FALSE )
     
      setwd( file.path(finaldir,state[i]) )
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
      
      # save: CAoverall.Rdata (in ST & LOGS folder)
      print("--------------------------------------------")
      print(paste(state[i]," (n = ", nrow(st.inv)-nrow(ctrl),")") )
      print("-------------------------------------")
      
      save(list= c(paste("readme",state[i],"overall.results", sep=""), "noyrs"),
           file= paste(state[i],"overall.Rdata",sep="") )
      print(paste("FILE SAVED: ",getwd(),"/",st,"overall.Rdata", sep=""))
      
      setwd( file.path(finaldir,state[i]) )
      save(list= c(paste("readme",state[i],"overall.results", sep=""), "noyrs"),
           file= paste(state[i],"overall.Rdata",sep="") )
      print(paste("FILE SAVED: ",getwd(),"/",st,"overall.Rdata", sep=""))
      
      print("--------------------------------------------")
      t_st_f <- date()
      print(paste("RM state (",state[i],")"," start: ", t_st_0, sep=""))
      print(paste("RM state (",state[i],")","   end: ", t_st_f, sep=""))
      print("--------------------------------------------")
    }
  }
}

#  Readem Runs
# ==========================================================


#  By STATE (53 incl. "National")
# ================================

setwd("/work/project/final")

st <- c(dir())


try( readem(state=st, boot=TRUE) )
t_st_f <- date()


#   By STATE*YR (53 incl. "National" * 2006~2015)
# ================================

setwd("/work/project/final")

st <- c(dir())
yrs <- c(as.character(2006:2015))

t_styr_0 <- date()
try( readem(state=st, yr=yrs, boot=TRUE) )
t_styr_f <- date()


#  SAVE AND LOG GLOBAL START/END TIMES
# ================================================
setwd("/work/project/LOGS")

t_f=date()

print("----------------------------------")
print(paste("RM.r started:", t_0))
print(paste("RM.r completed:", t_f))
print("----------------------------------")

sink()

q()

