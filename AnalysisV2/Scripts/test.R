recieveUserInput <- function(variable){
  if(variable == "window_length"){
    x <- readline(prompt = "Enter length of window (in sec): ")
  }
  else if(variable == "sample_rate"){
    x <- readline(prompt = "Enter accelerometer sampling frequency (in Hz): ")
  }
  return(x)
}

window_length <- recieveUserInput("window_length")
sample_rate <- recieveUserInput("sample_rate")