examples.distance.after.event = function() {
  x = c(0,0,1,0,0,0,1,0,0,1)  
  event.val = 1
  distance.after.event(x,1)
  x = sample(0:1,1000, replace=TRUE)
  distance.after.event(x,1)
  x
}

distance.after.event = function(x, event.val = TRUE, before=FALSE) {
  if (before) {
    ret = rev(distance.after.event(rev(x), event.val=event.val))
    return(ret)
  }
  is.event = (x == event.val)
  ind = seq_along(x)
  dist = ind - cummax(is.event*ind)
  
  # Set NA to first values
  first.event = which(is.event)[1]
  if (length(first.event)>0)
    if (first.event>1)
      dist[1:(first.event-1)] = NA
  dist
}