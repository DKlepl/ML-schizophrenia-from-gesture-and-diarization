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
    name = paste(info$ID, n, info$right, sep="_")
    
    save_path = "trash/"
    try(if (actigraph_subset$interviewer[1]==0) {
      save_path = "clean_data/Split_data/Single/Participant/"
    } else save_path = "clean_data/Split_data/Single/Interviewer/")
    
    save_as = paste0(save_path, name, ".csv")
    
    write.csv(actigraph_subset, save_as, row.names = F)
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
    
    if (f%%5 == 0) {
      print(paste(f, "out of 81 processed."))
    }
  }
}

#Coordination splitting - 2 utterances long
make_pairs = function(data, info) {
  #number the utterances (should correspond to utterance_n in actigraph)
  data$utterance_n = seq(1, nrow(data))
  
  #split by interlocutor
  diar_i = subset(data, Interlocutor=="Interviewer")
  diar_p = subset(data, Interlocutor == "Participant")
  
  #dataframe to store the pairs in
  pairs = data.frame(ID = numeric(),
                     utterance_1 = numeric(),
                     utterance_2 = numeric(),
                     latency = numeric())
  
  #search for closest utterance in time - pick one utterance in diar_i and compare it to all in diar_p
  for (n in 1:nrow(diar_i)) {
    one_utterance = diar_i[n,]
    
    #sometimes there might be 2 utterances to pair with, before and after the given utterance
    end_start_diff = abs(diar_p$StartTime-one_utterance$EndTime)
    following_utterance = diar_p[which.min(end_start_diff),]
    
    #append the pair to the pairs dataframe
    f_pair = data.frame(ID=info$ID,
                        utterance_1 = one_utterance$utterance_n,
                        utterance_2 = following_utterance$utterance_n,
                        latency = end_start_diff[which.min(end_start_diff)])
    pairs = rbind(pairs, f_pair)
    
    start_end_diff = abs(one_utterance$StartTime-diar_p$EndTime)
    previous_utterance = diar_p[which.min(start_end_diff),]
    
    #append the pair to the dataframe
    p_pair = data.frame(ID=info$ID,
                        utterance_1 = one_utterance$utterance_n,
                        utterance_2 = previous_utterance$utterance_n,
                        latency = start_end_diff[which.min(start_end_diff)])
    pairs = rbind(pairs, p_pair)
  }
  return(pairs)
}

coordination_split = function(filename) {
  inf = get_info(filename)
  diarization = read.csv(filename)
  diarization$utterance_n = seq(1, nrow(diarization))
  pairs = try(make_pairs(data = diarization, info = inf))
  
  return(pairs)
}

coordination_split_all = function() {
  all_diar = list.files("clean_data/Diarization", full.names = T)
  all_pairs = data.frame(ID = numeric(),
                         utterance_1 = numeric(),
                         utterance_2 = numeric(),
                         latency = numeric())
  for (i in 1:length(all_diar)) {
    file = all_diar[i]
    pairs = try(coordination_split(filename = file))
    all_pairs = try(rbind(all_pairs, pairs))
  }
  write.csv(all_pairs, "clean_data/Split_data/coordination_pairs.csv", row.names = F)
}

