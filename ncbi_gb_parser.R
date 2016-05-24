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
  mol_type <- str_extract(split.txt, "(?<=mol_type=\").*(?=\")")
  isolation_source <- gsub("\n                     "," ", 
                           str_extract(split.txt, "(?<=isolation_source=\").*(\n                     [A-Za-z0-9% ]+)*(?=\"\n                     )"))
  location <- gsub("\n                     "," ", 
                   str_extract(split.txt, "(?<=country=\").*(\n                     [A-Za-z0-9% ]+)*(?=\"\n[                          rRNA])"))
  product <- str_extract(split.txt, "(?<=product=\").*(?=\")")
  
  df <- data.frame('accession' <- accession,
                   'length' <- length,
                   'definition' <- definition,
                   'keyword' <- keyword,
                   'org_source' <- org_source,
                   'organism' <- organism,
                   'mol_type' <- mol_type,
                   'isolation_source' <- isolation_source,
                   'location' <- location,
                   'product' <- product)
  colnames(df) <- c('accession', 
                    'length',
                    'definition',
                    'keyword',
                    'org_source',
                    'organism',
                    'mol_type',
                    'isolation_source',
                    'location',
                    'product')
  return(df)
}
