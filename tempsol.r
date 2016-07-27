rm(list=ls())

# Annot.dir <- 'R:/StaffFolders/KonagayaEugene/Annotations/RMS' # error UCSC site down?
# Annot.dir <- 'R:/StaffFolders/KonagayaEugene/Annotations'
Annot.dir <- 'C:/Users/Eugene/Desktop/Catherine/Annotations'

setwd(Annot.dir)

source('rsid2annot.r')

setwd(file.path(Annot.dir,"output"))

CSVs <- list.files()
CSVs <- CSVs[grep(".*.csv", list.files())]
CSVs <- CSVs[grep(".*top.finding.*", CSVs)]
append(CSVs, list.files()[grep(".*5C.*.csv", list.files())])

#   IF > 300 RSIDs, split into smaller bits!
#    (a list of snp-sets [300 SNP batches])
# =================================================
for (i in 1:length(CSVs)) {
  
  filenom <-  gsub(".csv", "", CSVs[i])
  filenom <- substr(filenom, 6,19)
  
  CSV <- read.csv(CSVs[i])
  
  if (nrow(CSV) > 300) {  
    n <- nrow(CSV) %/% 300  # 4099 %/% 300 = 4
    rem.n <- nrow(CSV) %% 300
    
    CSV.spl <- vector("list", n+1)
    names(CSV.spl) <- c(1:(n+1))
    
    for (x in 0:(n-1)) {
      CSV.spl[[as.character(x+1)]] <- CSV[seq(300*x+1,300*(x+1)),]
    }
    CSV.spl[[n+1]] <- CSV[(300*n+1):(300*n+rem.n),]
    
  # IF <= 300 SNPs, convert to list-object
  } else {
    CSV.spl <- list(CSV)
    names(CSV.spl) <- 1
  }
  
  # RSID >> Annotation (in batches of 1K)
  for (setnum in names(CSV.spl)) {
    N <- length(names(CSV.spl))
    rsid2annot( CSV.spl[[setnum]][['name']], 
                paste0(filenom, " (", setnum, "of", N, ")") )
  }
}

