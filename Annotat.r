rm(list=ls())

Logfile <- paste0("Annotat.r.LOG_", Sys.Date(), ".txt")

# Annot.dir <- 'R:/StaffFolders/KonagayaEugene/Annotations/RMS' # error UCSC site down?
Annot.dir <- 'R:/StaffFolders/KonagayaEugene/Annotations'
# Annot.dir <- 'C:/Users/Eugene/Desktop/Catherine/Annotations'

setwd(Annot.dir)

file.create(Logfile)
sink(Logfile)


# # # IF packages not installed...
# # # =================================================
# install.packages('stringr', repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
# source("https://bioconductor.org/biocLite.R")
# # source("http://bioconductor.org/biocLite.R")
# biocLite()
# biocLite('rtracklayer', ask = F)
# biocLite('TxDb.Hsapiens.UCSC.hg19.knownGene', ask=F)
# biocLite('VariantAnnotation', ask = F)
# biocLite('GenomicFeatures', ask = F)
# # # ----------------------------------------
# # biocLite('ensemblVEP')
# # biocLite('AnnotationHub')
# # library(AnnotationHub)
# library(ensemblVEP)

source('rsid2annot.r')
require(rtracklayer)
require(stringr)
require(TxDb.Hsapiens.UCSC.hg19.knownGene)
require(VariantAnnotation)

hg19 <- TxDb.Hsapiens.UCSC.hg19.knownGene::TxDb.Hsapiens.UCSC.hg19.knownGene



# ================================================
#  "coord2snp" function
# ================================================
#  input: an Annotation directory that...
#            - contains .txt files w/ snp coordinates 
#                                 (e.g. "1:65922727:A:G")
# 
#  output: .csv files with corresponding rsid coordinates (via UCSC genome tables)
#

coord2snp <- function (parent.dir = Annot.dir) {
  
  setwd(parent.dir)
  
  folders <- list.dirs()
  folders <- folders[!folders %in% c(".", "./output")]       # rmv self-reference
  folders <- substr(folders, 3, nchar(folders)) # rmv "./" from the head of each folder reference
  
  if ( !dir.exists( file.path(Annot.dir, 'output') ) ) { 
    dir.create(file.path(parent.dir,"output") ) # create parent.dir/output
    rdata.files <- c()
  } else {
    #check what's been done already
    setwd( file.path(Annot.dir, 'output') )
    files <- list.files()
    rdata.files <- files[grep(".*.Rdata", files)] # list of .Rdata files
  }  
  for (i in 1:length(folders)) {
    
    setwd( file.path(parent.dir, folders[i]) )
    files <- list.files()                 # list of files & directories
    files <- files[grep(".*.txt", files)] # subset list to .txt files
    
    files <- files[!gsub(".txt", "", files) %in% gsub(".Rdata", "", rdata.files)]
    if (!length(files) > 0) {
      sprintf("NO NEW .TXT files in '%s'", folders[i])
    } else {
      for (j in 1:length(files)) {
        
        filename <- gsub(".txt$", "", files[j])   # filename sans-extension
        
        print( paste(files[j]) )
        print( paste("Time: ", as.character(date()) ) )
        
        #   READ TEXT FILE and EXTRACT GENOMIC COORDINATES of SNPs
        # ===========================================================
        # must contain "SNP" column !
        #   format e.g.  6:29855574:G:GCCCTGGCCCTGA
        
        orig <- read.table(files[j], header=FALSE, stringsAsFactors = FALSE)
        # snp-coord list (NO HEADER, 1-column, ASSUMES snp-coord in Correct Format!)
        if ( ncol(orig)==1 && !( ("SNP" %in% orig[1,]) || 
                                 ("snp" %in% orig[1,]) ) ) { 
          
          snp <- as.vector(orig$V1, mode="character")
          names(orig) = c('SNP')
          
          # snp-coord list (w/ HEADER, multi-column, "SNP"/"snp" in header req'd!)  
        } else if ("SNP" %in% orig[1,] || "snp" %in% orig[1,]) {
          
          orig <- read.table(files[j], header=TRUE)
          snp <- as.vector(orig$SNP)
          
          # NO "SNP"/"snp" in header!  
        } else {
          snp <- "No SNPs"                              
          print(paste("NO SNP-HEADER IN: ", files[j]))
        }
        
        # split components of SNP-coord
        # (e.g. 1:123124:A:G >> chm1, 123124, A:G )
        snp2 <- as.data.frame( str_split_fixed(snp, pattern=":",n=3) )
        snp2$V2 <- sapply(snp2$V2, as.character)
        snp2$V2 <- sapply(snp2$V2, as.numeric)
        
        # SNP coord = bp_f; grange = [bp_f-1, bp_f]
        snp2$bp_0 = snp2$V2-1
        
        snp2 <- snp2[,c(1,4,2,3)]
        
        # for checking purposes, add original SNP column
        # ----------------------------------------------------
        snp2 <- cbind(orig$SNP, snp2)
        colnames(snp2) <- c('SNP','chr', 'bp_0','bp_f', "a0:a1")
        # ----------------------------------------------------
        
        snp2$chr <- paste0("chr", snp2$chr)
        
        #   IF > 1000 SNPs, split into smaller bits!
        #   "snp2.spl[n]" (a list of snp-sets [1000 SNP batches])
        # =================================================
        if (nrow(snp2) > 1000) {  
          n <- nrow(snp2) %/% 1000  # 4099 %/% 1000 = 4
          rem.n <- nrow(snp2) %% 1000
          
          snp2.spl <- vector("list", n+1)
          names(snp2.spl) <- c(1:(n+1))
          
          for (x in 0:(n-1)) {
            snp2.spl[[as.character(x+1)]] <- snp2[seq(1000*x+1,1000*(x+1)),]
          }
          snp2.spl[[n+1]] <- snp2[(1000*n+1):(1000*n+rem.n),]
          
          # IF <= 1000 SNPs, convert to list-object
        } else {
          snp2.spl <- list(snp2)
          names(snp2.spl) <- '1'
        }
        
        
        #  get.RSIDs(snp2.spl) (get Tables from UCSC table browser)
        # =============================================================
        #  CREATE 'GRanges object' from SNP-coord
        # --------------------------------------------------
        # coords <- IRanges(snp2$bp_0, snp2$bp_f)
        
        for (setnum in names(snp2.spl)) {
          assign( paste0("coords",setnum),
                  IRanges(snp2.spl[[setnum]][["bp_0"]], snp2.spl[[setnum]][["bp_f"]] ) )
        }
        
        # targetRanges <- with(snp2.spl, GRangesForUCSCGenome("hg19", chr, coords, "+"))
        for (setnum in names(snp2.spl)) {
          assign( paste0("targetRanges.",setnum),
                  with(snp2.spl[[setnum]],GRangesForUCSCGenome("hg19", chr, get(paste0("coords",setnum)), "+")) )
        }
        
        #  START UCSC.GenomeBrowser Session
        # -------------------------------------------
        mySession <- browserSession("UCSC")
        genome(mySession) <- "hg19"
        
        #  QUERY UCSC.GB for 'rsids'
        # -------------------------------------------
        for (setnum in names(snp2.spl)) {
          
          assign( paste0("q.rsid.",setnum),
                  ucscTableQuery(mySession,                                 # session = mySession
                                 track="snp146",                            # track = "snp146"
                                 get(paste0("targetRanges.",setnum)) ) )    # range = "targetRanges.n"
        }
        # tableNames(q.rsid)
        
        finaltable <- paste0("tbl.q.rsid.", filename)
        
        # GET UCSC QUERY TABLE(S) >> tbl.q.rsid.'n' [ n = (1:max,5) ]
        for (setnum in names(snp2.spl)) {
          assign( paste0("tbl.q.rsid.",setnum),
                  getTable( get(paste0("q.rsid.",setnum)) ) )
        }
        
        # RBIND TO 1 TABLE ("tbl.q.rsid.[filename]" a.k.a. finaltable)
        if (length(names(snp2.spl)) == 1) {
          assign(finaltable,
                 tbl.q.rsid.1)
        } else {
          assign(finaltable,
                 tbl.q.rsid.1)
          
          for (setnum in names(snp2.spl)[2:length(names(snp2.spl))]) {
            assign(finaltable,
                   base::rbind( get(finaltable), 
                                get(paste0("tbl.q.rsid.", setnum)) ) )
          }
        }
        
        # for QC purposes !!!!!!!
        # ===================================================================
        # to check for SNP-coords w/ multi-rsids
        
        # counts: Frequency-table of SNP-coord in 'finaltable'
        counts.tabl <- table(get(finaltable)$chrom,get(finaltable)$chromEnd) # 'table' object
        counts <- as.data.frame(counts.tabl)                            # actual frequency-table
        names(counts) <- c("chrom", "chromEnd", "Freq")
        counts <- counts[counts$Freq>0,]
        
        counts.multi <- counts[counts$Freq>1,]
        
        #  save CSV to output (e.g. igf.snps.csv)
        # ----------------------------------------------------------
        setwd( file.path(parent.dir,"output") )
        
        toCSV <- data.frame(get(finaltable)$chrom, get(finaltable)$chromEnd, get(finaltable)$name)
        colnames(toCSV) <- c("chrom", "chromEnd","name")
        
        f <- paste0("rsid.",filename,".csv")
        file.create( f )
        
        write.csv( toCSV, row.names= FALSE, file= f)
        
        #   IF > 1000 RSIDs, split into smaller bits!
        #    (a list of snp-sets [1000 SNP batches])
        # =================================================
        if (nrow(toCSV) > 1000) {  
          n <- nrow(toCSV) %/% 1000  # 4099 %/% 1000 = 4
          rem.n <- nrow(toCSV) %% 1000
          
          toCSV.spl <- vector("list", n+1)
          names(toCSV.spl) <- c(1:(n+1))
          
          for (x in 0:(n-1)) {
            toCSV.spl[[as.character(x+1)]] <- toCSV[seq(1000*x+1,1000*(x+1)),]
          }
          toCSV.spl[[n+1]] <- toCSV[(1000*n+1):(1000*n+rem.n),]
          
          # IF <= 1000 SNPs, convert to list-object
        } else {
          toCSV.spl <- list(toCSV)
          names(toCSV.spl) <- 1
        }
        # RSID >> Annotation (in batches of 1K)
        
        N <- length(names(toCSV.spl))
        
        for (setnum in names(toCSV.spl)) {
          rsid2annot(toCSV.spl[[setnum]][['name']], paste0(filename," (",setnum,"of",N,")") )
        }
        
        setwd( file.path(parent.dir, 'output') )
        save(list=ls(), file=paste0(filename,".Rdata"))
        print("saving:")
        print(ls())
        # rm(list=ls()[!(ls() %in% 
        #                  c( 'coord2snp', 'hg19', 'i', 'j', 'parent.dir') )])
        
        # back to folder
        setwd( file.path(parent.dir, folders[i]) )
      }
    }
  }
}

#  START ANNOTATION PROCEDURE
# ============================
setwd(Annot.dir)
getwd()

#  Inventory
# -------------------------------------
folders <- list.dirs()
folders <- folders[!folders %in% c(".", "./output")]            # rmv self-reference
folders <- substr(folders, 3, nchar(folders)) # rmv "./" from the head of each folder reference

folders

#  RDATA-LIST:
# -----------------
if (!dir.exists( file.path(Annot.dir, 'output')  )) {
  print("Fresh Run: output not found")
  rdata.files <- c()
} else {
  setwd( file.path(Annot.dir, 'output') )
  files <- list.files()
  rdata.files <- files[grep(".*.Rdata", files)] # list of .Rdata files
  
  print ( rdata.files )
}

#  .TXT FILE-LIST:
# -------------------

for (i in 1:length(folders)) {
  
  setwd( file.path(Annot.dir, folders[i]) )
  files <- list.files()                 # list of files & directories
  rdata.files <- files[grep(".*.Rdata", files)] # list of .Rdata files
  files <- files[grep(".*.txt", files)] # subset list to .txt file
  
  for (j in 1:length(files)) {
    if (length(files) > 0) { print( paste("...", folders[i], files[j], sep="/") ) }
    if (length(rdata.files) > 0) { print( paste("...", folders[i], rdata.files[j], sep="/") ) }
  }  
}


#  RUN coord2snp()
# -------------------

coord2snp()

sink()



# shutdown
# os.shutdown(s=30)

# Useful References
# https://bioconductor.org/help/workflows/annotation/Genomic_Annotation_Resources/

# ideas
# ======================================
# OrgDb (for GO, KEGG, gene linking)
# TxDb 
# Uniprot
