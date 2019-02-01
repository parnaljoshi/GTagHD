isRefSeqRnaId <- function(id){
  require(stringr)
  flag <- stringr::str_detect(id, regex("^(NM|NR|XM|XR)_[0-9]{6}", ignore_case = TRUE))
  return(flag)                     
}

isRefSeqProtId <- function(id){
  require(stringr)
  flag <- stringr::str_detect(id, regex("^(AP|NP|YP|XP|WP)_[0-9]{6}", ignore_case = TRUE))
  return(flag)
}

isGenBankProtId <- function(id){
  require(stringr)
  flag <- stringr::str_detect(id, regex("^[A-Z]{3}[0-9]{5}", ignore_case = TRUE))
  return(flag)
}

isGenBankWGSId <- function(id){
  require(stringr)
  flag <- stringr::str_detect(id, regex("^[A-Z]{4}[0-9]{8, 10}", ignore_case = TRUE))
  return(flag)
}

isRefSeqGenomicId <- function(id){
  require(stringr)
  flag <- stringr::str_detect(id, regex("^(AC|NC|NG|NT|NW|NZ)_[0-9]{6}\\.[0-9]", ignore_case = TRUE))
  return(flag)
}

isGenBankDnaId <- function(id){
  require(stringr)
  flag <- stringr::str_detect(id, regex("^[a-zA-Z]{1}[0-9]{5}", ignore_case = TRUE))
  if(!flag){
    flag <- stringr::str_detect(id, regex("^[a-zA-Z]{2}[0-9]{6}", ignore_case = TRUE))
  }
  return(flag)
}

