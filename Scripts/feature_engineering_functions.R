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
    remove_columns = c(-3, -4)
    save_path = "clean_data/Split_data/Interviewer/"
  } else {
    i=0
    remove_columns = c(-1, -2)
    save_path = "clean_data/Split_data/Participant/"
  }
  
  data_subset = subset(data, interviewer==i)
  data_subset = data_subset[,remove_columns]
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
    
    write.csv(dataframe, save_as, row.names = F)
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
  try(write.csv(all_losses, "clean_data/Split_data/data_loss_single.csv", row.names = F))
  
  return(all_losses)
}

#Coordination pairs
remove_no_match = function(){
  #copy contents of 'Interviewer' folder into 'Coordination/Interviewer_copy'
  original_interviewer = "clean_data/Split_data/Interviewer"
  copy_interviewer = "clean_data/Split_data/Coordination/Interviewer_copy"
  i_files = list.files(original_interviewer, full.names = T)
  i_copy_success = file.copy(i_files, copy_interviewer)
  sum(i_copy_success==F)
  
  #copy contents of 'Participant' folder into 'Coordination/Participant_copy'
  original_participant = "clean_data/Split_data/Participant"
  copy_participant = "clean_data/Split_data/Coordination/Participant_copy"
  p_files = list.files(original_participant, full.names = T)
  p_copy_success = file.copy(p_files, copy_participant)
  sum(p_copy_success==F)
  
  #load all files from the 'Interviewer' and 'Participant' folders inside of Coordination folder
  interviewer_files = list.files("clean_data/Split_data/Coordination/Interviewer_copy")
  participant_files = list.files("clean_data/Split_data/Coordination/Participant_copy")
  
  no_match_i = interviewer_files[(interviewer_files %in% participant_files) ==F]
  no_match_p = participant_files[(participant_files %in% interviewer_files) ==F]
  
  #sanity check
  length(interviewer_files)-length(no_match_i) == length(participant_files)-length(no_match_p)
  
  #delete files from both directories
  for (i in no_match_i) {
    folder = "clean_data/Split_data/Coordination/Interviewer_copy/"
    to_remove = paste0(folder, i)
    try(file.remove(to_remove, recursive = T, force = T),silent = T)
  }
  
  for (i in no_match_p) {
    folder = "clean_data/Split_data/Coordination/Participant_copy/"
    to_remove = paste0(folder, i)
    try(file.remove(to_remove, recursive = T, force = T), silent = T)
  }
}

