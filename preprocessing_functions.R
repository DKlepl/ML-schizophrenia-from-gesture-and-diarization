#read audio files in the folder
load_folder = function(path) {
  path = path
  all_files = list.files(path=path, full.names = T) 
  return(all_files)
}

get_info = function (f) {
  #subject ID
  filename = strsplit(f, '/')[[1]][3]
  id = strsplit(filename, '[.]')[[1]][1]
  
  #save also diagnosis
  if (startsWith(id,"1")==T) {
    diagnosis=1
  } else {
    diagnosis=0
  }
  info = data.frame(ID=as.numeric(id), diagnosis=diagnosis)
  return(info)
}

read_audio = function(f) {
  audio = tuneR::readWave(f)
  test = audio@left[(0):(10*22050)]
  return(test)
}

segment_audio = function(sound) {
  
  #use soundgen's algorithm to find bursts of acoustic energy in the signal
  #it's intended for syllable segmentation but it works well
  seg = soundgen::segment(sound, samplingRate = 22050,windowLength = 40, plot =F)
  bursts = seg$bursts
  
  #how to identify which burst are claps?
  #calculate SD with a sliding window - the lowest = 2 bursts small interburst interval
  sd= evobiR::SlidingWindow("sd",bursts$interburstInt , 2, 1)
  sd = c(sd,rep(NA,2))
  bursts$sd = sd
  return(bursts)
}

#plot the found peaks to make allow sanity check
plot_seg = function(audio, seg_audio) {
  windowLength_points = ceiling(40 * 22050 / 1000)
  
  sound_downsampled = seewave::env(
    audio,
    f = 22050,
    envt = "hil",
    msmooth = c(windowLength_points, 80),
    fftw = FALSE,
    plot = FALSE
  )
  timestep = 1000 / 22050 * (length(audio) / length(sound_downsampled))
  envelope = data.frame(time = ((1:length(
    sound_downsampled
  ) - 1) * timestep), value = sound_downsampled)
  
  p = plot(
    x = envelope$time,
    y = envelope$value,
    type = "l",
    col = "green",
    xlab = "Time, ms",
    ylab = "Amplitude"
  )
  points(seg_audio,
         col = "red",
         cex = 3,
         pch = 8)
}

#let the algoeithm choose the first clap and perform sanity check
sanity_check = function(seg) {
  choice = which.min(seg[,4])
  
  #construct checking question for user
  ask = paste0("I picked the ", choice, "th red cross as the first clap. What would be your choice, sir? ")
  check="no response"
  
  check = as.numeric(readline(ask))
  
  if (choice != check) {
    choice = check
  }
  return(choice)
}

#get the time of all the claps and construct an output
get_output = function(inf, segmented, first_clap) {
  three_claps = data.frame(audioclap1 = segmented[first_clap, 1], 
                           audioclap2 = segmented[first_clap+1, 1], 
                           audioclap3 = segmented[first_clap+2, 1])
  #get the time in seconds
  three_claps = three_claps/1000
  output = cbind(inf, three_claps)
  return(output)
}

get_claps = function(f) {
  info = get_info(f)
  audio = read_audio(f)
  extracted = segment_audio(sound = audio)
  plot_seg(audio, extracted)
  final_choice = sanity_check(extracted)
  one_file = get_output(inf=info, 
                        segmented = extracted, 
                        first_clap = final_choice)
  return(one_file)
}

preprocess_audio = function (folder_path) {
  audio_files = load_folder(folder_path)
  #place to store all claps
  audio_claps = data.frame()
  
  for (file in audio_files) {
    one_output = try(get_claps(file))
    
    #rbind the output to the big dataframe
    audio_claps = try(rbind(audio_claps, one_output))
  }
  
  #save the output
  write.csv(audio_claps, "clean_data/audio_claps.csv")
}