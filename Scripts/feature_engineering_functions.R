#Single signal data splitting - 10 seconds long
read_actigraph = function(filename) {
  data = read.csv(filename)
  data = data[,-1]
  
  return(data)
}

get_info = function(file) {
  filename = strsplit(file, "/")[[1]][3]
  split = strsplit(filename, "_")[[1]]
  info = data.frame(ID = split[1],
                    right = split[2])
  
  return(info)
}

split_by_interlocutor = function (data, info, interlocutor = c("I", "P"), sampling=100, split_lenght=10) {
  if (interlocutor=="I") {
    i=1
    save_path = "clean_data/Split_data/Interviewer/"
  } else {
    i=0
    save_path = "clean_data/Split_data/Participant/"
  }
  
  data_subset = subset(data, interviewer==i)
  rownames(data_subset)=NULL
  full_lenght = nrow(data_subset)
  
  split = sampling*split_lenght
  n_splits = trunc((nrow(data_subset)/split),0)
  
  data_subset = data_subset[1:(n_splits*split),]
  loss = (full_lenght-(nrow(data_subset)))/sampling
  
  data_subset$split = rep(1:n_splits,each=split)
  splitted = split(data_subset, data_subset$split)
  
  for (d in 1:length(splitted)) {
    dataframe = splitted[[d]]
    filename = paste(info$ID, info$right, d, ".csv", sep = "_")
    save_as = paste0(save_path, filename)
    
    write.csv(dataframe, save_as)
  }
  
  return(loss)
}

split_actigraph = function(filename) {
  actigraph = try(read_actigraph(filename))
  file_info = try(get_info(filename))
  loss_interviewer = try(split_by_interlocutor(data=actigraph, info = file_info, interlocutor = "I"))
  loss_participant = try(split_by_interlocutor(data=actigraph, info = file_info, interlocutor = "P"))
  
  loss = try(as.data.frame(cbind(loss_interviewer, loss_participant)))
  
  return(loss)
}

split_all = function(folder="clean_data/Gesture") {
  all_files = list.files(folder, full.names = T)
  all_losses = data.frame()
  for (i in 1:length(all_files)) {
    file = all_files[i]
    loss = try(split_actigraph(file))
    all_losses = try(rbind(all_losses, loss))
    }
  try(write.csv(all_losses, "clean_data/Split_data/data_loss_single.csv"))
  
  return(all_losses)
}
