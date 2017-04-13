getTitle <- function(dev=dev.cur()) {
    all_pointers <- getWindowsHandles(which="R", minimized=TRUE)
    all_pointers <- sapply(all_pointers, deparse)
    to_find <- deparse(getWindowsHandle(dev))
    if (to_find=="NULL") {
        NULL
    } else {
        names(all_pointers)[to_find==all_pointers]
    }
}
wait <- function(wait.time = 0.5){
  now <- proc.time()[3]
  while(proc.time()[3] < (now + wait.time)) dum <- 0
}