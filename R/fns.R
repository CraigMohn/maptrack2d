##  note that lag_one/lead_one pad the new entry with the first/last value,

lag_one <- function(vec) {
  return(c(vec[1],vec[-length(vec)]))
}
lead_one <- function(vec) {
  return(c(vec[-1],vec[length(vec)]))
}
dateTimeStr <- function(intDate,intTime) {
  return(paste0(stringr::str_pad(intDate,8,pad="0"),
                stringr::str_pad(intTime,6,pad="0")))
}
