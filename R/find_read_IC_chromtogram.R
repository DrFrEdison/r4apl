find.read.IC.chromtogram <- function(ID){
  
  txt.files <- dir( paste0(wd$team_labor$datensicherung$IC, "txt/"), pattern = "*.txt$")
  txt.sub <- substr(txt.files, 1, nchar(ID))
  txt.found.file <- txt.files[ txt.sub %in% gsub("\\:", "\\#", ID) ]
  txt.found.file <- txt.found.file[ length( txt.found.file )]
  
  raw.data <- fread(paste0(wd$team_labor$datensicherung$IC, "txt/", txt.found.file), header = F, encoding = "Latin-1"
                    , fill = T, sep = ";")
  
  datetime <- as.POSIXct( substr(as.character( raw.data[1, ] ), 1, 19), tz = "UTC")
  Ident <- raw.data[2, ]
  sample.ID <- raw.data[3, ]
  
  if(sample.ID != ID) warning("txt ID is not csv ID")
  
  
  rows.without.numbers <- raw.data$V1[ !grepl("[0-9]", raw.data$V1)]
  
  if(!any(c(any(raw.data$V1 %in% "Anionen"), any(raw.data$V1 %in% "Druck")))) stop("Neither Anionen-Chromatogram nor Druck found")
  
  if(!any(raw.data$V1 %in% "Anionen")) warning("Anionen not found in txt file")  
  
  if(any(raw.data$V1 %in% "Anionen")){
    
    if(!any(raw.data$V1 %in% "Druck")) Chromatogram <- raw.data[ (which(raw.data$V1 %in% "Anionen") + 1) : nrow(raw.data) , ] else{
      Chromatogram <- raw.data[ (which(raw.data$V1 %in% "Anionen") + 1) : (which(raw.data$V1 %in% "Druck") - 1) , ]}
    
    unknown.Chromatogram <- Chromatogram[1]
    column_names <- unlist(strsplit(as.character(Chromatogram$V1[2]), ";"))
    Chromatogram <- Chromatogram[-c(1, 2), ]
    Chromatogram <- Chromatogram[, tstrsplit(V1, ";", type.convert = TRUE)]
    setnames(Chromatogram, column_names)
    attr(Chromatogram, "unknown.Chromatogram") <- unknown.Chromatogram
    
    if(any( is.na(Chromatogram[ , 1]))) Chromatogram <- Chromatogram[ which(!is.na(Chromatogram[ , 1])) , ]
    if(any( is.na(Chromatogram[ , 2]))) Chromatogram <- Chromatogram[ which(!is.na(Chromatogram[ , 2])) , ]
    
  }
  
  if(!any(raw.data$V1 %in% "Druck")) warning("Druck not found in txt file")  
  if(any(raw.data$V1 %in% "Druck")){
    
    Druck <- raw.data[ (which(raw.data$V1 %in% "Druck") + 1) : nrow(raw.data) , ]
    unknown.Druck <- Druck[1]
    column_names <- unlist(strsplit(as.character(Druck$V1[2]), ";"))
    Druck <- Druck[-c(1, 2), ]
    Druck <- Druck[, tstrsplit(V1, ";", type.convert = TRUE)]
    setnames(Druck, column_names)
    attr(Druck, "unknown.Druck") <- unknown.Druck
    
    if(any( is.na(Druck[ , 1]))) Druck <- Druck[ which(!is.na(Druck[ , 1])) , ]
    if(any( is.na(Druck[ , 2]))) Druck <- Druck[ which(!is.na(Druck[ , 2])) , ]
    
  }
  
  
  returnlist <- list(datetime, Ident, sample.ID, if(exists("Druck")) Druck, if(exists("Chromatogram")) Chromatogram )
  names(returnlist) <- c("datetime", "Ident", "sample.ID", "Druck", "Chromatogram")
  
  returnlist <- returnlist[ !unlist(lapply(returnlist, is.null))]
  
  return(returnlist)
}
