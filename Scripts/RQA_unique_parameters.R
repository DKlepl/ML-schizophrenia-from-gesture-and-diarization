source("Scripts/feature_engineering_functions.R")

get_parameters = function(n) {
  folder_c = "clean_data/Split_data/rqa_c/"
  folder_p = "clean_data/Split_data/rqa_p/"
  folder_i = "clean_data/Split_data/rqa_i/"
  format = ".csv"
  
  param_average = read.csv("clean_data/Split_data/final_parameters.csv", row.names = 1)
  
  param_c = paste0(folder_c, n, format)
  param_p = paste0(folder_p, n, format)
  param_i = paste0(folder_i, n, format)
  
  param_c = try(read.csv(param_c))
  param_p = try(read.csv(param_p))
  param_i = try(read.csv(param_i))
  
  if (class(param_c)=="try-error") {
    param_c = param_average["coordination",]
  }
  if (class(param_p)=="try-error") {
    param_p = param_average["single",]
  }
  if (class(param_i)=="try-error") {
    param_i = param_average["single",]
  }
  
  param = rbind(param_c, param_p, param_i)
  rownames(param) = c("c", "p", "i")
  
  return(param)
}

extract_rqa = function(data, parameters) {
  rqa_interviewer = as.data.frame(compute_rqa(data[,1], data[,1], 
                                              radius = parameters["i","radius"],
                                              emddim = parameters["i","emddim"],
                                              delay = parameters["i","delay"]))
  rqa_participant = as.data.frame(compute_rqa(data[,2], data[,2], 
                                              radius = parameters["p","radius"],
                                              emddim = parameters["p","emddim"],
                                              delay = parameters["p","delay"]))
  rqa_coordination = as.data.frame(compute_rqa(data[,1], data[,2], 
                                               radius = parameters["c","radius"],
                                               emddim = parameters["c","emddim"],
                                               delay = parameters["c","delay"]))
  
  output = list(rqa_interviewer, rqa_participant, rqa_coordination)
  
  return(output)
}

extract_features = function(file, n) {
  info = get_info2(file)
  data = read.csv(file)
  utterances_features = try(extract_utterance_features(data))
  descriptive_features = try(extract_descriptive_features(data))
  param = get_parameters(n)
  rqa_features = try(extract_rqa(data, parameters = param))
  
  interviewer_features = try(cbind(info, 
                                   utterances_features[[1]],
                                   descriptive_features[[1]],
                                   rqa_features[[1]], param["i",]))
  participant_features = try(cbind(info,
                                   utterances_features[[2]],
                                   descriptive_features[[2]],
                                   rqa_features[[2]],
                                   param["p",]
                                   ))
  coordination_features = try(cbind(info,
                                    utterances_features[[3]],
                                    descriptive_features[[3]],
                                    rqa_features[[3]],
                                    param["c",]))
  
  output = list(interviewer_features, participant_features, coordination_features)
  
  return(output)
}

extract_features_folder = function(folder) {
  all_files = list.files(folder, full.names = T)
  
  n=1
  
  interviewer_data = data.frame()
  participant_data = data.frame()
  coordination_data = data.frame()
  
  for (file in all_files) {
    print(n)
    all_features = try(extract_features(file = file, n=n))
    
    
    interviewer_data = rbind(interviewer_data, all_features[[1]])
    participant_data = rbind(participant_data, all_features[[2]])
    coordination_data = rbind(coordination_data, all_features[[3]])
    
    write.csv(interviewer_data, "clean_data/ML_ready/interviewer_data_unique.csv", row.names = F)
    write.csv(participant_data, "clean_data/ML_ready/participant_data_unique.csv", row.names = F)
    write.csv(coordination_data, "clean_data/ML_ready/coordination_data_unique.csv", row.names = F)
    
    if (n%%200 == 0) {
      print(paste(n, "out of 7028 files processed."))
    }
    n=n+1
  }
}

extract_features_folder(folder="clean_data/Split_data/Coordination")

