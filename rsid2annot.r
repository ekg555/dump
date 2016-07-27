require(RSelenium)

remDr <- remoteDriver(remoteServerAddr = "localhost", browser = "chrome")

checkForServer()
Sys.sleep(1)

startServer()
Sys.sleep(1)
remDr$open()
remDr$close()

# setwd("C:/Users/Eugene/Desktop/") # for laptop
# setwd('C:/Users/Eugene/Desktop/Catherine/') # for home-desktop
setwd("C:/Users/!eugene_konagaya/Desktop")

#  "rsid2annot()" function
# ==============================
#  input: takes a vector of rsids
#
#  controls chrome & downloads annotation files from UCSC Varriant Annotation Integrator
#  

rsid2annot <- function(rsids=NULL, file=NULL) {
  remDr <- remoteDriver(remoteServerAddr = "localhost", browser = "chrome")
  
  checkForServer()
  startServer()
  remDr$open()
  remDr$navigate("https://genome.ucsc.edu/cgi-bin/hgVai")
  
  # TEST: rsids <- read.table('hgTables.txt') 
  rsid <- as.vector(rsids, mode='character')
  
  #  SELECT GENOME ASSEMBLY & REGION
  # ---------------------------------
  Mammal.select <- remDr$findElement(using = 'xpath', "//form[4]/span[1]//select/option[1]")            # clade = mammal
  Mammal.select$clickElement()
  # Sys.sleep(.02)
  
  genome.select <- remDr$findElement(using = 'xpath', "//form[4]/span[2]//select/option[1]")            # genome = human
  genome.select$clickElement()
  # Sys.sleep(.02)
  
  hg19.select <- remDr$findElement(using = 'xpath', "//form[4]/span[3]//select/option[2]")              # assembly = hg19
  hg19.select$clickElement()
  # Sys.sleep(.5)
  
  annotate.genome.select <- remDr$findElement(using = 'xpath', "//form[4]/span[4]//select/option[1]")   # region_to_annotate = genome
  annotate.genome.select$clickElement()
  # Sys.sleep(.02)
  
  #  SELECT VARIANTS
  # ---------------------------------
  # /html/body/table/tbody/tr/td/div[1]/table/tbody/tr/td/table/tbody/tr/td/table/tbody/tr[2]/td[2]/form[4]/select[1]/option[2]
  varID.select <- remDr$findElement(using = 'xpath', "//form[4]/select[1]/option[2]")
  varID.select$clickElement()
  # Sys.sleep(.5)
  
  #  SELECT GENES
  # ---------------------------------
  #/html/body/table/tbody/tr/td/div[1]/table/tbody/tr/td/table/tbody/tr/td/table/tbody/tr[2]/td[2]/form[4]/select[3]/option[1] # UCSC Genes
  
  
  #  SELECT MORE ANNOTATIONS (OPTIONAL)
  # ---------------------------------
  
  #  CONFIGURE OUTPUT
  # ---------------------------------
  outTXT.select <- remDr$findElement(using = 'xpath', "//form[4]/select[4]/option[1]") # output format = VEP (tab-delim txt)
  outTXT.select$clickElement()
  # Sys.sleep(.05)
  
  #  SELECT MAX VARIANTS = 1000
  # ===================================
  #/html/body/table/tbody/tr/td/div[1]/table/tbody/tr/td/table/tbody/tr/td/table/tbody/tr[2]/td[2]/form[4]/select[2]
  maxVar.select <- remDr$findElement(using = 'xpath', "//form[4]/select[2]/option[3]")
  maxVar.select$clickElement()
  # Sys.sleep(.05)

  
  #  TO PREVENT "REQUEST URI TOO LARGE" ERROR
  # ======================================
  url <- remDr$getCurrentUrl()
  url <- paste0(url, "&formMethod=POST")
  remDr$navigate(url)
  # Sys.sleep(.5)
  
  #  ENTER RSIDs & FILENAME
  # ===============================

  #xpath for textbox (for rsid)
  #/html/body/table/tbody/tr/td/div[1]/table/tbody/tr/td/table/tbody/tr/td/table/tbody/tr[2]/td[2]/form[4]/div[3]/textarea
  rsid.txtbox <- remDr$findElement(using = 'xpath', "//form[4]/div[3]/textarea")
  rsid.txtbox$sendKeysToElement( list(as.character(paste(rsid, collapse ="\n"))) )
  
  # humanized wait
  # Sys.sleep(mean(sample(3:5,10, replace=TRUE)))
  
  # name saved file
  outTXT.txtbox <- remDr$findElement(using = 'xpath', "//form[4]/input[4]")
  outTXT.txtbox$clickElement()
  Sys.sleep(.05)
  outTXT.txtbox$sendKeysToActiveElement(list(file))

  # humanized wait
  # Sys.sleep(mean(sample(5:7,10, replace=TRUE)))
  
  
  #//form[4]/input[6]
  Get.gzip <- remDr$findElement(using = 'xpath', "//form[4]/input[6]")
  Get.gzip$clickElement()

  if(length(rsid) < 500) {
    #//form[4]/input[5]
    Get.txt <- remDr$findElement(using = 'xpath', "//form[4]/input[5]")
    Get.txt$clickElement()
    
    outTXT.txtbox <- remDr$findElement(using = 'xpath', "//form[4]/input[4]")
    outTXT.txtbox$clickElement()
    outTXT.txtbox$sendKeysToActiveElement(list(key= "end", ".txt"))
  }

  
  #//form[4]/input[7]
  Get.Results1 <- remDr$findElement(using = 'xpath', "//form[4]/input[7]")
  Get.Results1$clickElement() 
  
  
  #/html/body/div[2]/div[11]/div/button[2]
  Get.Results2 <- remDr$findElement(using = 'xpath', "//div[11]/div/button[2]")
  Get.Results2$clickElement() 
  
  sprintf( "Annotation Query for %s Started: %s", filename, Sys.time())
  
}
