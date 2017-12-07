###
###
### This is a parser for files downloaded from ncbi.nlm.nih.gov in format of "GenBank (full)" specifically
### require(stringr)
###
require(stringr)
ncbi_gb_parser <- function(gb_file) { # gb_file: directory to the sequence.gb.txt file
  
  # 
  file <- readChar(gb_file, file.info(gb_file)$size)
  
  split.txt <- unlist(strsplit(file, "\n\n"))
  
  definition <- gsub("\n            "," ", 
                     str_extract(split.txt, "(?<=\nDEFINITION  ).*(\n            .*)*"))
  accession <- str_extract(split.txt, "(?<=\nACCESSION   ).*")
  length <- str_extract(split.txt, "(?<=source          1..)[0-9]*")
  keyword <- str_extract(split.txt, "(?<=\nKEYWORDS    ).*")
  org_source <- str_extract(split.txt, "(?<=\nSOURCE      ).*")
  organism <- gsub("\n            "," ", 
                   str_extract(split.txt, "(?<=\n  ORGANISM  ).*(\n            .*)*"))
  title <- gsub("\n            "," ", 
                   str_extract(split.txt, "(?<=\n  TITLE  ).*(\n            .*)*"))
  mol_type <- str_extract(split.txt, "(?<=mol_type=\").*(?=\")")
  isolation_source <- gsub("\n                     "," ", 
                           str_extract(split.txt, "(?<=isolation_source=\").*(\n                     [A-Za-z0-9% ]+)*(?=\"\n                     )"))
  location <- gsub("\n                     "," ", 
                   str_extract(split.txt, "(?<=country=\").*(\n                     [A-Za-z0-9% ]+)*(?=\"\n[                          rRNA])"))
  lat_lon <- str_extract(split.txt, "(?<=lat_lon=\").*(?=\")")
  product <- str_extract(split.txt, "(?<=product=\").*(?=\")")
  
  df <- data.frame('accession' <- accession,
                   'length' <- length,
                   'definition' <- definition,
                   'keyword' <- keyword,
                   'org_source' <- org_source,
                   'organism' <- organism,
                   'title' <- title,
                   'mol_type' <- mol_type,
                   'isolation_source' <- isolation_source,
                   'location' <- location,
                   'lat_lon' <- lat_lon,
                   'product' <- product)
  colnames(df) <- c('accession', 
                    'length',
                    'definition',
                    'keyword',
                    'org_source',
                    'organism',
                    'title',
                    'mol_type',
                    'isolation_source',
                    'location',
                    'lat_lon',
                    'product')
  return(df)
}
