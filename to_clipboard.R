to_clipboard <- function(my_df) {
  clip <- pipe("pbcopy", "w")                       
  write.csv(my_df, file=clip)                               
  close(clip)
}
