#custom function for creating index for repeatedCV while keeping participant§s samples within one fold together
make_folds = function (ID, k , rep) {
  index = list()
  #list of 10 folds ==first round of CV
  imp_group = ID
  
  for (n in 1:rep) {
    one_rep = groupKFold(imp_group, k=k)
    names(one_rep) = paste0(names(one_rep), ".Rep", n)
    index = c(index, one_rep)
  }
  
  return(index)
}

#read and transform data into caret-y type
caret_friendly = function(data) {
  #store variables not used for models in separate df
  info_cols = c("ID", "right", "time")
  no_model = data[, info_cols]
  no_model$set = data$set
  
  #get folds indexes
  index = make_folds(ID=subset(no_model, set=="train")$ID, k=10, rep=30)
  
  #and remove this columns from data
  data[, info_cols] = lapply(data[, info_cols], as.null)
  
  #all columns except for diagnosis and set should be numeric
  set = c(1, (which(colnames(data) == "set")))
  num_cols = colnames(data)[-set]
  data[, num_cols] = lapply(data[, num_cols], as.numeric)
  
  #relevel diagnosis so that Schizophrenia is first
  data$diagnosis = relevel(data$diagnosis, ref = "Schizophrenic")
  levels(data$diagnosis)
  
  #train-test split
  train = subset(data, set == "train")
  test = subset(data, set == "test")
  test$set = NULL
  train$set = NULL
  
  caret_friendly = list(
    train = train,
    test = test,
    no_model = no_model,
    index = index
  )
  
  return(caret_friendly)
}


#load all trained models
load_models = function(which=c("C", "P")) {
  
  if (which=="C") path= "Models/Coordination"
  if (which=="P") path= "Models/Participant"
  
  models_path = list.files(path, full.names = T)
  models_list = list()
  model_names = list.files(path)
  model_names = strsplit(model_names, "\\.")
  for (n in 1:length(models_path)) {
    model = models_path[n]
    name = model_names[[n]][1]
    models_list[[name]]=readRDS(model)
  }
  
  return(models_list)
}