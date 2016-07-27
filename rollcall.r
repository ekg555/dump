rm(list=ls())

## capture all the output to a file.
setwd("/work/project/LOGS")
file.create("inventory.Rout", overwrite = TRUE)
zz <- file("/work/project/LOGS/inventory.Rout", open = "wt")
sink(zz)

# ===========================================================
#  INVENTORY.R
# ===========================================================
# creates "inventory" data-frame
# w/ 1 observation per UNQ (unique article-id)
#
# Variables:
# - UNQ
# - state
# - yr
# - file.temp
# - ndoc.temp
# - file.final
# 

library(XML)

tempdir <- "/work/project/temp"
finaldir <- "/work/project/final"


# INITIALIZE VARS
# ===============================================

state <- c()
yr <- c()
file.temp <- c()
file.final <- c()
UNQ <- c()

temp.inventory <- data.frame(state, yr, file.temp, UNQ)
final.inventory <- data.frame(state, yr, file.final, UNQ)


# INVENTORY TEMP
# ===============================================

ptm <- proc.time()

setwd(tempdir)
states <- list.files()

# $state
for (i in 1:length(states)) {
  setwd( file.path(tempdir,states[i]) )
  years <- list.files()
  
  # $year
  for (j in 1:length(years)) {
    setwd( file.path(tempdir,states[i],years[j]) )
    files <- list.files()
    
    # empty-file handling
    info=file.info(files)
    empty.temp=rownames(info[info$size==0,])
    
    # $file
    for (l in 1:length(files)) {
      if (!files[l] %in% empty.temp) {
        top <- xmlRoot(xmlTreeParse(files[l]))
        
        # GET UNQ(s) & Add to Inventory
        for ( m in 1:length(names(top)) ) {
          UNQ <- xmlValue(top[[m]][["UNQ"]])
          rbind(temp.inventory, data.frame(states[i], years[j], files[i], UNQ))
          }
        } else {
          rbind(temp.inventory, data.frame(states[i], years[j], files[i], 0))
        }
    }
  }
}

proc.time() - ptm
t_1=date()

# INVENTORY FINAL
# ===============================================

ptm <- proc.time()

setwd(finaldir)
states <- list.files()

# $state
for (i in 1:length(states)) {
  setwd( file.path(finaldir,states[i]) )
  years <- list.files()
  
  # $year
  for (j in 1:length(years)) {
    setwd( file.path(finaldir,states[i],years[j]) )
    files <- list.files()
    
    # empty-file handling
    info=file.info(files)
    empty.final=rownames(info[info$size==0,])
    
    # $file
    for (l in 1:length(files)) {
      if (!files[l] %in% empty.final) {
        top <- xmlRoot(xmlTreeParse(files[l]))
        
        # final$state$year$files$UNQ
          for ( m in 1:length(names(top)) ) {
            mth.UNQ <- xmlValue(top[["UNQ"]])
            rbind(final.inventory, data.frame(states[i], years[j], files[l], UNQ))
            }
        } else {
          rbind(final.inventory, data.frame(states[i], years[j], files[i], 0))
        }
    }
  }
}

proc.time() - ptm
t_2=date()

# MERGE INVENTORIES BY UNQ
# ===============================================

ptm <- proc.time()

try( inventory <- merge(final.inventory, temp.inventory, by.x="UNQ", by.y="UNQ", all = TRUE) )

proc.time() - ptm


# SAVE AND LOG TIMES
# ================================================
setwd("/work/project/LOGS")
file.create("rollcall.rda", overwrite=TRUE)
try( save(list=c('temp.inventory', 'final.inventory', 'inventory'), file="rollcall.rda") )

t_f=date()

print(paste("rollcall.r started:", t_0))
print(paste("inventory of 'temp' completed:", t_1))
print(paste("inventory of 'final' completed:", t_2))

print(paste("inventory completed:", t_f))

q()

