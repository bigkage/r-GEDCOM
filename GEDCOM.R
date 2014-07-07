library(stringr)
library(gtools)

gcdf <- data.frame(matrix(nrow=1, ncol=3)) 
names(gcdf) <- c("namelast","namefirst","birthdate")

dfTemp <- data.frame(matrix(nrow=1, ncol=3)) 
names(dfTemp) <- c("namelast","namefirst","birthdate")

rectype0 <- "z"
rectype1 <- "z"
rectype2 <- "z"
rectype3 <- "z"

conn <- file("export-Descendants.ged", "r")
while(length(line <- readLines(conn, 1)) > 0) {
#   print(cat("--------|", line))
  line <- str_trim(line, side = "both")
#   print(line)
  firstnum <- str_extract(line,"[0-9]")
  rectype <- str_extract(line, "ABBR|ADDR|ADR1|ADR2|ADOP|AFN|AGE|AGNC|ALIA|ANCE|ANCI|ANUL|ASSO|AUTH|BAPL|BAPM|BARM|BASM|BIRT|BLES|BLOB|BURI|CALN|CAST|CAUS|CENS|CHAN|CHAR|CHIL|CHR|CHRA|CITY|CONC|CONF|CONL|CONT|COPR|CORP|CREM|CTRY|DATA|DATE|DEAT|DESC|DESI|DEST|DIV|DIVF|DSCR|EDUC|EMIG|ENDL|ENGA|EVEN|FAM|FAMC|FAMF|FAMS|FCOM|FILE|FORM|GEDC|GIVN|GRAD|HEAD|HUSB|IDNO|IMMI|INDI|INFL|LANG|LEGA|MARB|MARC|MARL|MARR|MARS|MEDI|NAME|NATI|NATU|NCHI|NICK|NMR|NOTE|NPFX|NSFX|OBJE|OCCU|ORDI|ORDN|PAGE|PEDI|PHON|PLAC|POST|PROB|PROP|PUBL|QUAY|REFN|RELA|RELI|REPO|RESI|RESN|RETI|RFN|RIN|ROLE|SEX|SLGC|SLGS|SOUR|SPFX|SSN|STAE|STAT|SUBM|SUBN|SURN|TEMP|TEXT|TIME|TITL|TRLR|TYPE|VERS|WIFE|WILL")
#   print(recType)
  id <- str_extract(line,"@.+@")
  
#start new individual record
  if(firstnum == 0) {
    gcdf <- smartbind(gcdf, dfTemp, fill=NA)
    rectype0 <- rectype
  } else if(firstnum == 1) {
    rectype1 <- rectype
  } else if(firstnum == 2) {
    rectype2 <- rectype
  } else if(firstnum == 3) {
    rectype3 <- rectype
  }

#name info
  if(rectype1 == "NAME" && rectype2 == "GIVN") {
    dfTemp$namefirst <- str_extract(line, "\\d\\sGIVN\\s(.+)")
  }

#handle birthdate info
  if(rectype1 == "BIRT" && rectype2 =="DATE") {
    dfTemp$birthdate = str_extract(line, "\\d{2}\\s\\[A-Z]{3}\\s\\d{4}")
  }

}