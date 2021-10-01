Stats_fun <- function (df){
  # This function relies on the samples to be introduced in the data as Sample_ID
 if ("NPOC_mg_C_per_L"%in% colnames(df) == TRUE){
   # Adding a column for just sample names, no reps
   data.stats = as.data.frame(matrix(NA,ncol = 8, nrow = nrow(df)))
   colnames(data.stats) = c("Sample_ID","Sample_name","NPOC_CV","NPOC_variance","NPOC_range","TN_CV","TN_variance","TN_range")
   data.stats$Sample_ID = df$Sample_ID
   # Removing the replicates from the sample ID
   data.stats$Sample_name = data.stats$Sample_ID
   data.stats$Sample_name = gsub(pattern = "-1","",data.stats$Sample_name)
   data.stats$Sample_name = gsub(pattern = "-2","",data.stats$Sample_name)
   data.stats$Sample_name = gsub(pattern = "-3","",data.stats$Sample_name)
   
   unique.sample.names = unique(data.stats$Sample_name)
   
   # Calculate stats for each unique sample
   for (i in 1:length(unique.sample.names)){
     index = grep(unique.sample.names[i],data.stats$Sample_name)
     npoc.temp = df$NPOC_after_correcting_for_dilution[grep(unique.sample.names[i],data.stats$Sample_name)]
     tn.temp = df$TN_after_correcting_for_dilution[grep(unique.sample.names[i],data.stats$Sample_name)]
     data.stats$NPOC_CV[index] = sd(npoc.temp) / mean(npoc.temp) * 100
     data.stats$NPOC_range[index] =  max(npoc.temp) - min(npoc.temp)
     data.stats$NPOC_variance[index] = var(npoc.temp)
     
     data.stats$TN_CV[index] = sd(tn.temp) / mean(tn.temp) * 100
     data.stats$TN_range[index] =  max(tn.temp) - min(tn.temp)
     data.stats$TN_variance[index] = var(tn.temp)
     
   }
   
   return(hist(npoc.tn.combined$NPOC_CV, breaks = 55))
   hist(npoc.tn.combined$NPOC_range, breaks = 55)
   hist(npoc.tn.combined$NPOC_variance, breaks = 55)
  return(data.stats)
 }else{
    ions = names(df)[2:ncol(df)]
    data.stats = as.data.frame(matrix(NA,ncol = ((length(ions)*3)+2), nrow = nrow(df)))
    #Creating column names
   name = c("Sample_ID","Sample_name")
    for (i in 1:length(ions)){
      a = paste0(ions[i],"_CV")
      b = paste0(ions[i],"_range")
      c = paste0(ions[i],"_variance")
      name = c(name,a,b,c)
    }
  colnames(data.stats) = name
  data.stats$Sample_ID = df$Sample_ID
  # Removing the replicates from the sample ID
  data.stats$Sample_name = data.stats$Sample_ID
  data.stats$Sample_name = gsub(pattern = "-1","",data.stats$Sample_name)
  data.stats$Sample_name = gsub(pattern = "-2","",data.stats$Sample_name)
  data.stats$Sample_name = gsub(pattern = "-3","",data.stats$Sample_name)
  
  unique.sample.names = unique(data.stats$Sample_name)
  
for (j in 2:length(ions)){  
  df1 = cbind.data.frame(Sample_ID = df$Sample_ID,df[grep(pattern = ions[j],x = colnames(df))])
  for (i in 1:length(unique.sample.names)){
    index = grep(unique.sample.names[i],data.stats$Sample_name)
    temp = df1[grep(unique.sample.names[i],data.stats$Sample_name),2]
   
     data.stats[index,grep(pattern = paste0(ions[j],"_CV"),x = colnames(data.stats))] = sd(temp) / mean(temp) * 100
    data.stats[index,grep(pattern = paste0(ions[j],"_range"),x = colnames(data.stats))] =  max(temp) - min(temp)
    data.stats[index,grep(pattern = paste0(ions[j],"_variance"),x = colnames(data.stats))] = var(temp)
    
    
  }
} 
  
  }

}