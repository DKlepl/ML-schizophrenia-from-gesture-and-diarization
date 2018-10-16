#Single signal data split - 1 utterance long
load_actigraph = function(file) {
  data = read.csv(file)
  data = data[,-1]
  
  #remove silence
  data = subset(data, interviewer !=-1)
  
  return(data)
}

get_info = function(file) {
  filename = strsplit(file, "/")[[1]][3]
  split = strsplit(filename, "_")[[1]]
  info = data.frame(ID = split[1],
                    right = split[2])
  
  return(info)
}

split_save_single = function (data, info) {
  for (n in unique(data$utterance_n)) {
    actigraph_subset = subset(data, utterance_n==n)
    
    #construct name of the file to save
    duration = nrow(actigraph_subset)
    name = paste(info$ID, info$right, n , duration, sep="_")
    
    save_path = "trash/"
    try(if (actigraph_subset$interviewer[1]==0) {
      save_path = "clean_data/Split_data/Single/Participant/"
    } else save_path = "clean_data/Split_data/Single/Interviewer/")
    
    save_as = paste0(save_path, name, ".csv")
    
    write.csv(actigraph_subset, save_as)
  }
}

split_single = function(file) {
  data = try(load_actigraph(file))
  info = try(get_info(file))
  try(split_save_single(data=data, info=info))
}

split_single_all = function() {
  all_gest = list.files("clean_data/Gesture", full.names = T)
  for (f in 1:length(all_gest)) {
    file = all_gest[f]
    try(split_single(file))
  }
}
