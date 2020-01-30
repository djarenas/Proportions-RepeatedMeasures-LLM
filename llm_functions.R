organize_by_label <- function(file, group_name, tpoints){
  #Input. File you want to read. Rows should be patients, columns different time points.
  #group_name as a string. t_points is a numerical vector of the time independend variables
  #Output: A data frame easier for input into ANOVA
  prop <- pull(file, c(1:(length(tpoints))))  #Pull the proportion data at all time points from b_file
  if (negative_values_allowed == FALSE){
    z = clr_only_two(prop)                     #Calculate centered-log-transform    
  } else {z = prop*0}
  n_patients <- length(prop[[1]])             
  label_organized <- NULL
  for (i in 1:length(prop)){
    p1 <- prop[[i]]
    z1 <- z[, i]
    dummyf <- data.frame(group = rep(group_name,n_patients), time = rep(tpoints[i], n_patients), patient = c(1:n_patients), proportion = p1, z = z1)
    label_organized <- rbind(label_organized, dummyf)
  }
  return(label_organized)
}