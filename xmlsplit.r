
# install.packages('XML')
library(XML)

# ====================================
#  FOLDER STRUCTURE COPY TO "FINAL"
# ====================================

t_0=date()

setwd("/work/project")
dir.create("final")

setwd("temp")
states <- list.files()

for (i in 1:length(states)) {
  setwd(states[i])
  #/work/project/temp/CA/ (GET YEAR.LIST)
  if (states[i] == "National") {
    y_0=9
    y_f=12
  } else {
    y_0=3
    y_f=6
  }
  year.list <- unique(substr(list.files(), y_0, y_f))
  #/work/project/final/ (CREATE STATE FOLDER)
  setwd("/work/project/final/")
  dir.create(states[i])
  setwd(states[i])
  
  for (j in 1:length(year.list)) {
    #/work/project/final/CA (CREATE YEAR FOLDERS)
    dir.create(year.list[j])
  }  
  setwd("/work/project/temp")
}


# =============================================
#  RE-ORGANIZE FOLDER STRUCT IN "TEMP"
# =============================================

for (i in 1:length(states)) {
  setwd(states[i])
  xml.filelist <- list.files()
  if (states[i] == "National") {
    y_0=9
    y_f=12
  } else {
    y_0=3
    y_f=6
  }
  year.list <- unique(substr(list.files(), y_0, y_f))

  for (j in 1:length(year.list)) {
    dir.create(year.list[j])
    
    for (k in 1:length(xml.filelist)) {
      if (substr(xml.filelist[k], y_0, y_f)==year.list[j]) {
	file.rename(xml.filelist[k], file.path(year.list[j], xml.filelist[k]))
      }
    }
  }
  setwd("/work/project/temp")
}

# =============================================
#  SPLIT DOCS IN XML & SAVE AS TXT IN "FINAL"
# =============================================

t_1=date()

dir.create("/work/project/LOGS")
setwd("/work/project/LOGS")
file.create("empties.txt")
empties="/work/project/LOGS/empties.txt"

write("EMPTY FILES\n===========", file=empties, append=TRUE)
write(date(), file=empties, append=TRUE)

setwd("/work/project/temp")
states <- list.files()

#STATE
for (i in 1:length(states)) {
  setwd(states[i])
  #/work/project/temp/CA/
  year.list <- list.files()
  
  for (j in 1:length(year.list)) {   
    #(re)set counter
    n=1
    setwd(year.list[j])
    
    #/work/project/temp/CA/2007/
    xml.filelist <- list.files()
    info= file.info(xml.filelist)
    # FIND EMPTY FILES & LOG
    empty = rownames(info[info$size == 0, ])
    write(paste(getwd(), "\n=======", sep=""), file=empties, append=TRUE)    

    # extract title & text
    
    for (k in 1:length(xml.filelist)) {
      if (!xml.filelist[k] %in% empty) {
        xml.filename <- xml.filelist[k]    
        xml.file <- xmlTreeParse(xml.filename) # translates file into R object
        top <- xmlRoot(xml.file, useInternalNodes=TRUE)
    
        for (l in 1:length(names(top)) ) {
     	
	  #EXTRACT & PROCESS l-th DOC
#          setwd(file.path("/work/project/SCRIPTS"))
#         saveXML(top[[l]], file="temp.xml")

#	  temp <- readLines("temp.xml")
#	  temp <- gsub("<p/>", "\n", temp, fixed=TRUE)
#          writeLines(temp, "temp.xml")
	  

#          ttl <- xmlValue(top[[l]][["title"]])
#          txt <- grep("<p/>", "\n", xmlValue(top[[l]][["maintext"]]))
#          doc <- paste(ttl, "\n\n", txt)
      
          setwd(file.path("/work/project/final", states[i], year.list[j]))
          filenom <- paste(states[i], sprintf("%05d",n), ".txt", sep="")
#          write(doc, file=filenom)

  	  saveXML(top[[l]], file=filenom)
          setwd(file.path("/work/project/temp", states[i], year.list[j]))        
          n = n + 1
        
          }
      } else {
          write(xml.filelist[k], file=empties, append=TRUE)
      }

    }
    setwd("..")
    
  }
  setwd("..")
}


t_f=date()

print(paste("xmlsplit started:", t_0))
print(paste("folder struct completed:", t_1))
print(paste("completed:", t_f))

# XPATH Dx
#=====================================
# names(top[[1]])
# names(top[[1]][["title"]])
# names(top[[1]][["title"]][["text"]])
# top[[1]][["title"]][["text"]]


