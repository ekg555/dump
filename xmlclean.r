
# install.packages('XML')
library(XML)

t_0=date()

setwd("/work/project")
dir.create("final.clean")

# ========================================
#   COPY "FINAL" TO "FINAL.CLEAN"
# ========================================

setwd("final")
states <- list.files()

for (i in 1:length(states)) {
  setwd(states[i])
  #/work/project/temp/CA/ (GET YEAR.LIST)
  year.list <- unique(substr(list.files(), 3, 6))
  
  #/work/project/final/ (CREATE STATE FOLDER)
  setwd("/work/project/final/")
  dir.create(states[i])
  setwd(states[i])
  
  for (j in 1:length(year.list)) {
    #/work/project/final/CA (CREATE YEAR FOLDERS)
    dir.create(year.list[j])
  }  
  setwd("/work/project/final")
}



# =============================================
#  CLEAN TXT IN "FINAL.CLEAN"
# =============================================

t_1=date()

setwd("/work/project/final.clean")
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


