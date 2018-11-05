#Single signal data splitting - 10 seconds long
read_actigraph = function(filename) {
  data = read.csv(filename)
  data = data[,-1]
  data = na.omit(data)
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

merge_signals = function(filename) {
  #folders where all data splits are
  interviewer_folder = "clean_data/Split_data/Coordination/Interviewer_copy/"
  participant_folder = "clean_data/Split_data/Coordination/Participant_copy/"
  
  #construct the name of the 2 data splits
  interviewer_file = paste0(interviewer_folder, filename)
  participant_file = paste0(participant_folder, filename)
  
  data_interviewer = read.csv(interviewer_file)
  data_participant = read.csv(participant_file)
  
  #remove and rename columns to have unique column names in both dataframes
  data_interviewer = data_interviewer[,c(-4, -6)]
  colnames(data_interviewer) = c("PsychologistJerkLeft", "PsychologistJerkRight", "time_int", "utterance_n_int")
  
  data_participant = data_participant[,c(-4,-6)]
  colnames(data_participant) = c("ParticipanttJerkLeft", "ParticipantJerkRight", "time_par", "utterance_n_par")
  
  coordination_pair = cbind(data_interviewer, data_participant)
  
  #order the columns nicer
  coordination_pair = coordination_pair[,c(1,2,5,6,3,7,4,8), drop=F]
  
  #is the participant right- or left-handed?
  right = as.numeric(strsplit(filename,"_")[[1]][2])
  
  #keep only signal from the dominant hand of participant, unless that signal includes more than 200 zeroes, then keep the other hand signal
  if (right==1) {
    if (sum(coordination_pair$ParticipantJerkRight==0)<500) {
      coordination_pair = coordination_pair[,-3]
    } else coordination_pair = coordination_pair[,-4]
  } else {
    if (sum(coordination_pair$ParticipanttJerkLeft==0)<500) {
      coordination_pair = coordination_pair[,-4]
    } else coordination_pair = coordination_pair[,-3]
  }
  
  #keep only the signal from dominant hand of the psychologist (which is always right) unless the signal contains more than 200 zeroes, then keep the other hand signal
  if (sum(coordination_pair$PsychologistJerkRight==0)<500) {
    coordination_pair = coordination_pair[,-1]
  } else coordination_pair = coordination_pair[,-2]
  
  #save the resulting file
  save_file = paste0("clean_data/Split_data/Coordination/", filename)
  write.csv(coordination_pair, save_file, row.names = F)
}

create_coordination_pairs = function() {
  all_filenames = list.files("clean_data/Split_data/Coordination/Interviewer_copy")
  
  for (f in all_filenames) {
    try(merge_signals(filename = f))
  }
  
  print("Done.")
}

#Feature extraction

#get info about the participant and data split
get_info2 = function(file) {
  filename = strsplit(file, "/")[[1]][4]
  split = strsplit(filename, "_")[[1]]
  ID = split[1]
  if ((substr(ID,1,1))==1){
    diagnosis = 1
  } else diagnosis = 0
  info = data.frame(ID = ID,
                    diagnosis = diagnosis,
                    right = split[2],
                    split_n = split[3])
  return(info)
}

#extract features about utterances
compute_utterance_features = function (duration) {
  utterance_stats = pastecs::stat.desc(duration$duration)
  utterances_features = data.frame(n_utterances = utterance_stats[1],
                                   median_duration = utterance_stats[8],
                                   mean_duration = utterance_stats[9],
                                   min_duration = utterance_stats[4],
                                   max_duration = utterance_stats[5],
                                   range_duration = utterance_stats[6],
                                   sd_duration = utterance_stats[13],
                                   iqr_duration = IQR(duration$duration))
  return(utterances_features)
}

extract_utterance_features = function(data) {
  library(tidyverse)
  duration_int = data %>% group_by(utterance_n_int) %>% summarise(duration = max(time_int)-min(time_int))
  duration_par = data %>% group_by(utterance_n_par) %>% summarise(duration = max(time_par)-min(time_par))
  
  colnames(duration_int)[1] = 'utterance_n'
  colnames(duration_par)[1] = 'utterance_n'
  
  duration_both = rbind(duration_int, duration_par)
  
  features_int = compute_utterance_features(duration_int)
  features_par = compute_utterance_features(duration_par)
  features_cor = compute_utterance_features(duration_both)

  output = list(features_int, features_par, features_cor)
  return(output)
}

#to extract statistical features the signal needs to be transformed

  #for coordination pair - coerce the signals into one vector so that features can be computed
coerce_signals = function(data) {
  signal = c(data[,1], data[,2])
  return(signal)
}

#function for extracting statistical features from the signal
compute_descriptive_features = function (signal) {
  descriptive_features = as.data.frame(t(pastecs::stat.desc(signal)))
  descriptive_features = descriptive_features[,c(-1,-3,-10,-11,-14)]
  colnames(descriptive_features)[9] = "sd"
  descriptive_features$iqr = IQR(signal, na.rm = T)
  descriptive_features = cbind(descriptive_features,as.data.frame(t(quantile(signal))))
  descriptive_features$mad = mad(signal)
  
  return(descriptive_features)
}

extract_descriptive_features = function (data) {
  signal_interviewer = data[,1]
  signal_participant = data[,2]
  signal_coordination = coerce_signals(data)
  
  desc_interviewer = compute_descriptive_features(signal_interviewer)
  desc_participant = compute_descriptive_features(signal_participant)
  desc_coordination = compute_descriptive_features(signal_coordination)
  
  output = list(desc_interviewer, desc_participant, desc_coordination)
  return(output)
}

#run RQA
## optimize parameters for one file function
optimize_par = function(ts1, ts2) {
  initial_par = list(lgM =  20, steps = seq(1, 6, 1), 
                     radiusspan = 200, radiussample = 10, 
                     normalize = 1, rescale = 1, mindiagline = 2, 
                     minvertline = 2, tw = 0, whiteline = FALSE, 
                     recpt = FALSE, fnnpercent = 10, typeami = "mindip")
  
  optim_par = crqa::optimizeParam(ts1, ts2, par = initial_par, min.rec = 3.5, max.rec = 4.5)
  optim_par = as.data.frame(optim_par)
  return(optim_par)
}

#select one common set of parameters
##for single data - can use coordination pairs
run_optimization = function(folder) {
  all_files = list.files(folder, full.names = T)
  n= 1

  for (file in all_files) {
    data = read.csv(file)
    print(file)
    participant_param = try(optimize_par(ts1 = data[,2], ts2 = data[,2]))
    interviewer_param = try(optimize_par(ts1 = data[,1], ts2 = data[,1]))
    coordination_param = try(optimize_par(ts1 = data[,1], ts2 = data[,2]))
    
    name = paste0(n, ".csv")
     
    try(write.csv(participant_param, paste0("clean_data/Split_data/rqa_p/", name), row.names = F))
    try(write.csv(interviewer_param, paste0("clean_data/Split_data/rqa_i/", name), row.names = F))
    try(write.csv(coordination_param, paste0("clean_data/Split_data/rqa_c/", name), row.names = F))
    
    n = n+1
  }
}

merge_parameters = function(folder) {
  all = list.files(folder, full.names = T)
  all_parameters = data.frame()
  
  for (file in all) {
    one = try(read.table(file, header=T, sep = ","))
    all_parameters = try(rbind(all_parameters, one))
  }
  return(all_parameters)
}

merge_parameters_all = function() {
  parameters_c = merge_parameters(folder = "clean_data/Split_data/rqa_c/")
  write.csv(parameters_c,"clean_data/Split_data/parameters_coordination.csv", row.names = F)
  
  parameters_p = merge_parameters(folder = "clean_data/Split_data/rqa_p/")
  write.csv(parameters_p,"clean_data/Split_data/parameters_participant.csv", row.names = F)
  
  parameters_i = merge_parameters(folder = "clean_data/Split_data/rqa_i/")
  write.csv(parameters_i,"clean_data/Split_data/parameters_interviewer.csv", row.names = F)
}

select_parameters = function() {
  par_par = read.csv("clean_data/Split_data/parameters_participant.csv")
  par_int = read.csv("clean_data/Split_data/parameters_interviewer.csv")
  par_cor = read.csv("clean_data/Split_data/parameters_coordination.csv")
  
  #merge participant and interviewer
  par_single = rbind(par_par, par_int)
  
  par_single$radius = as.numeric(as.character(par_single$radius))
  par_single$emddim = as.numeric(as.character(par_single$emddim))
  par_single$delay = as.numeric(as.character(par_single$delay))
  
  par_cor$radius = as.numeric(as.character(par_cor$radius))
  par_cor$emddim = as.numeric(as.character(par_cor$emddim))
  par_cor$delay = as.numeric(as.character(par_cor$delay))
  
  par_single = na.omit(par_single)
  par_cor = na.omit(par_cor)
  
  final_parameters = data.frame(emddim = c(median(par_single$emddim),median(par_cor$emddim)),
                                delay = c(median(par_single$delay),median(par_cor$delay)),
                                radius = c(median(par_single$radius),median(par_cor$radius)))
  
  rownames(final_parameters) = c("single", "coordination")
  
  write.csv(final_parameters,"clean_data/Split_data/final_parameters.csv")
}

compute_rqa = function(ts1, ts2, radius, emddim, delay) {
  result = crqa::crqa(ts1, ts2, 
                      radius = radius, embed = emddim, delay = delay,
                      normalize = 1, rescale = 1, mindiagline = 2, minvertline = 2)
  
  result = result[-10]
  return(result)
}

extract_rqa = function(data, parameters) {
  rqa_participant = as.data.frame(compute_rqa(data[,2], data[,2], 
                                radius = parameters["single","radius"],
                                emddim = parameters["single","emddim"],
                                delay = parameters["single","delay"]))
  rqa_interviewer = as.data.frame(compute_rqa(data[,1], data[,1], 
                                radius = parameters["single","radius"],
                                emddim = parameters["single","emddim"],
                                delay = parameters["single","delay"]))
  rqa_coordination = as.data.frame(compute_rqa(data[,1], data[,2], 
                                radius = parameters["coordination","radius"],
                                emddim = parameters["coordination","emddim"],
                                delay = parameters["coordination","delay"]))
  
  output = list(rqa_participant, rqa_interviewer, rqa_coordination)
  
  return(output)
}

extract_features = function(file) {
  info = get_info2(file)
  data = read.csv(file)
  utterances_features = extract_utterance_features(data)
  descriptive_features = extract_descriptive_features(data)
  
  interviewer_features = cbind(info, 
                               utterances_features[[1]],
                               descriptive_features[[1]])
  participant_features = cbind(info,
                               utterances_features[[2]],
                               descriptive_features[[2]])
  coordination_features = cbind(info,
                                utterances_features[[3]],
                                descriptive_features[[3]])
}
