Binary_on_surface <- function(){ # local.path,intermediate.path,plot_data.path,figure.path,s1200.path,template.fname
  # the original tutorial is in https://mvpa.blogspot.com/2017/11/assigning-arbitrary-values-to-surface.html
  rm(list=ls());  # clear R's memory
  
  setwd("C:/Users/home/Downloads/workbench-windows64-v1.5.0/workbench/bin_windows64/"); # location of wb_command.exe, to call wb_command functions via system() within R  
  
  local.path <- "C:/Holobrain/data/Mapping_MMP/";  # local directory for reading and writing template files
  intermediate.path <- "C:/Holobrain/data/Mapping_MMP/intermediate_binary_result/";
  plot_data.path <- "C:/Holobrain/data/Mapping_MMP/plot_data/";
  figure.path <- "C:/Holobrain/data/Mapping_MMP/figure/";
  s1200.path <- "C:/Holobrain/data/Mapping_MMP/"; # HCP S1200 Group Average Data/Single Sample Data Release
  template.fname <- "MMPtemplate.pscalar.nii";  # filename for the template we'll make
  
  # make a template pscalar.nii from the MMP atlas  # the group dsalar.nii filename is S1200.thickness_MSMAll.32k_fs_LR.dscalar.nii
  system(paste0("wb_command -cifti-parcellate ", s1200.path, "Q1-Q6_RelatedParcellation210.thickness_MSMAll_2_d41_WRN_DeDrift.32k_fs_LR.dscalar.nii ", 
                s1200.path, "Q1-Q6_RelatedValidation210.CorticalAreas_dil_Final_Final_Areas_Group_Colors.32k_fs_LR.dlabel.nii COLUMN ", 
                local.path, template.fname));  
  if (!file.exists(paste0(local.path, template.fname))) { stop(paste0("missing: ", local.path, template.fname)); }  
  
  # you can make a text version of the template, which has 360 rows, but the values in each row aren't the parcel numbers.
  # system(paste0("wb_command -cifti-convert -to-text ", local.path, template.fname, " ", local.path, "temp.txt"));
  
  # the text file needs to be arranged with the 180 right hemisphere parcels in the first 180 rows (in order),
  # then the 180 parcels for the left hemisphere.
  # build a text file with the values to plot.
  
  # collect all the binary mapping coefficient of all task comparisons 'merge.data'
  setwd(intermediate.path)
  
  a = list.files(path = intermediate.path, pattern = "\\.txt$")
  dir = paste("./",a,sep="")
  n = length(dir)
  merge.data = read.table(a[1],header=F) # ,sep=""   
  
  # merge.data = abs(merge.data)
  if (dim(merge.data)[2]==360) {
    merge.data = t(merge.data)
  } else {
    merge.data = t(t(merge.data))
  }
  
  exert_n = n + dim(merge.data)[2] - 1
  if (dim(merge.data)[2]>1) {
    for (char_i in 2:dim(merge.data)[2]) {
      a <- c(a[1:(char_i-1)],paste(char_i,"_",a[1],sep=""),a[char_i:length(a)])
    }
  }
  
  for (j in 1:dim(merge.data)[2]) {
    select_max_index <- order(merge.data[,j],decreasing=TRUE)[1:20]
    select_index <- select_max_index
    # select_min_index <- order(merge.data[,j],decreasing=FALSE)[1:10]
    # select_index <- c(select_max_index,select_min_index)
    for (k in 1:dim(merge.data)[1]) {
      if (!(k %in% select_index)) {
        merge.data[k,j]<-0
      }
    }
  }
  
  for (i in 2:n){
    new.data = read.table(a[exert_n-n+i], header=F) # , sep=","
    # new.data = abs(new.data)
    
    if (dim(new.data)[2]==360) {
      new.data = t(new.data)
    } else {
      new.data = t(t(new.data))
    }
    
    if (dim(new.data)[2]>1) {
      exert_n = exert_n + dim(new.data)[2] - 1
      insert_pos = length(a)-n+i
      if (i==n) {
        for (char_i in 2:dim(new.data)[2]) {
          a <- c(a[1:(char_i+insert_pos-2)],paste(char_i,"_",a[insert_pos],sep=""))
        }
      } else {
        for (char_i in 2:dim(new.data)[2]) {
          a <- c(a[1:(char_i+insert_pos-2)],paste(char_i,"_",a[insert_pos],sep=""),a[(char_i+insert_pos-1):length(a)])
        }
      }
    }
    
    for (j in 1:dim(new.data)[2]) {
      select_max_index <- order(new.data[,j],decreasing=TRUE)[1:20]
      select_index <- select_max_index
      # select_min_index <- order(new.data[,j],decreasing=FALSE)[1:10]
      # select_index <- c(select_max_index,select_min_index)
      for (k in 1:dim(new.data)[1]) {
        if (!(k %in% select_index)) {
          new.data[k,j]<-0
        }
      }
    }
    
    merge.data = cbind(merge.data,new.data)
  }
  # write.csv(merge.data,file = "./merge_only_csv.csv",row.names=FALSE)  
  
  # create the .pscalar.nii file
  setwd("C:/Users/home/Downloads/workbench-windows64-v1.5.0/workbench/bin_windows64/");
  
  print(a)
  tmp <- strsplit(a, split = ".", fixed = TRUE)
  original_filename <- unlist(lapply(tmp, head, 1))
  for (i in 1:exert_n){
    out.vec <- rep(0,360);  
    for (j in 1:360){
      out.vec[j] <- merge.data[j,i];
    }
    # out.vec[c(1,10,15)] <- 1;  # right hemisphere parcels given value 1  
    # out.vec[c(180+1,180+10,180+15)] <- 2;  # corresponding left hemisphere parcels given value 2  
    # write.table(out.vec, paste0(plot_data.path, "plotDemo", substr(a[i],25,28), ".txt"), col.names=FALSE, row.names=FALSE);  
    write.table(out.vec, paste0(plot_data.path, "plotDemo", original_filename[i], ".txt"), col.names=FALSE, row.names=FALSE);  
    
    # create a CIFTI from the text file for viewing in Workbench  
    # system(paste0("wb_command -cifti-convert -from-text ", plot_data.path, "plotDemo", substr(a[i],25,28), ".txt ", local.path, template.fname, " ", figure.path, "plotDemo", substr(a[i],25,28), ".pscalar.nii"));  
    # if (!file.exists(paste0(figure.path, "plotDemo", substr(a[i],25,28), ".pscalar.nii"))) { stop(paste("missing:", figure.path, "plotDemo", substr(a[i],25,28), ".pscalar.nii")); }  
    system(paste0("wb_command -cifti-convert -from-text ", plot_data.path, "plotDemo", original_filename[i], ".txt ", local.path, template.fname, " ", figure.path, "plotDemo", original_filename[i], ".pscalar.nii"));  
    if (!file.exists(paste0(figure.path, "plotDemo", original_filename[i], ".pscalar.nii"))) { stop(paste("missing:", figure.path, "plotDemo", original_filename[i], ".pscalar.nii")); }  
  }
}

Binary_on_surface()