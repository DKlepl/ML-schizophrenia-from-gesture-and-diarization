source("Scripts/preprocessing_functions.R")

#detect claps in the audio recordings
audio_path = "raw_data/Audio"
try(get_claps_folder(folder_path = audio_path, type = "audio"))

#detect claps in the actigraph data
gesture_path = "raw_data/Gesture"
try(get_claps_folder(folder_path = gesture_path, type = "actigraph")) #works on 64 out of 86 files

#align the files and tag speaker in the actigraph data
try(tag_all())
