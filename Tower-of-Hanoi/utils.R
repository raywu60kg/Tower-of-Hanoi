tower_status = list(
  first_bar_status = c("small_ring", "medium_ring", "big_ring"),
  second_bar_status = c(),
  third_bar_status = c(),
  point_position = c("first_bar"),
  hold_ring = c(),
  ring_weight = list(
    small_ring = 1,
    medium_ring = 2,
    big_ring = 3
  )
)

hold <- function(tower_status){
  if(is.null(tower_status$hold_ring) == FALSE){
    tower_status$hold_ring = c()
  }else{
    switch(
      tower_status$point_position,
      first_bar = 
        if(is.null(tower_status$first_bar_status) == TRUE){
          next
        }else{
          tower_status$hold_ring = tower_status$first_bar_status[1]},  
      second_bar = 
        if(is.null(tower_status$second_bar_status) == TRUE){
          next
        }else{
          tower_status$hold_ring = tower_status$second_bar_status[1]},
      third_bar = 
        if(is.null(tower_status$third_bar_status) == TRUE){
          next
        }else{
          tower_status$hold_ring = tower_status$third_bar_status[1]},
    )}
  return(tower_status)
}


move_left <- function(tower_status){
  
  if(tower_status$point_position == "first_bar"){
    next
  }else{
    # move without hold
    if(is.null(tower_status$hold_ring) == TRUE){
      switch(tower_status$point_position,
        second_bar = (tower_status$point_position = "first_bar"),
        third_bar = (tower_status$point_position = "second_bar")
      )
    }else{
      # move with hold
      current_ring_weight = tower_status$ring_weight[tower_status$hold_ring]
      
      if(tower_status$point_position == "second_bar"){
        # from second to first bar
        if(is.null(tower_status$first_bar_status) == TRUE){
          target_ring_weight = 4
        }else{
          target_ring_weight = tower_status$first_bar_status[1]
        }
        if(current_ring_weight > target_ring_weight){
          next 
        }else{
          tower_status$point_position = "first_bar"
          tower_status$second_bar_status = tower_status$second_bar_status[-1]
          tower_status$first_bar_status = c(tower_status$hold_ring, tower_status$first_bar_status)
        }
      }else{
        # from third to second bar
        if(is.null(tower_status$second_bar_status) == TRUE){
          target_ring_weight = 4
        }else{
          target_ring_weight = tower_status$second_bar_status[1]
        }
        if(current_ring_weight > target_ring_weight){
          next 
        }else{
          tower_status$point_position = "second_bar"
          tower_status$third_bar_status = tower_status$third_bar_status[-1]
          tower_status$second_bar_status = c(tower_status$hold_ring, tower_status$second_bar_status)
        }
      }
    }
  }
        
  return(tower_status)
}
        

move_right <-function(tower_status){
  
  if(tower_status$point_position == "third_bar"){
    next
  }else{
    # move without hold
    if(is.null(tower_status$hold_ring) == TRUE){
      switch(tower_status$point_position,
             first_bar = (tower_status$point_position = "second_bar"),
             second_bar = (tower_status$point_position = "third_bar")
      )
    }else{
      # move with hold
      current_ring_weight = tower_status$ring_weight[tower_status$hold_ring]
      
      if(tower_status$point_position == "first_bar"){
        # move from first to second
        if(is.null(tower_status$second_bar_status) == TRUE){
          target_ring_weight = 4
        }else{
          target_ring_weight = tower_status$second_bar_status[1]
        }
        if(current_ring_weight > target_ring_weight){
          next 
        }else{
          tower_status$point_position = "second_bar"
          tower_status$first_bar_status = tower_status$first_bar_status[-1]
          tower_status$second_bar_status = c(tower_status$hold_ring, tower_status$second_bar_status)
        }
      }else{
        # move from second to third
        if(is.null(tower_status$third_bar_status) == TRUE){
          target_ring_weight = 4
        }else{
          target_ring_weight = tower_status$third_bar_status[1]
        }
        if(current_ring_weight > target_ring_weight){
          next 
        }else{
          tower_status$point_position = "third_bar"
          tower_status$second_bar_status = tower_status$second_bar_status[-1]
          tower_status$third_bar_status = c(tower_status$hold_ring, tower_status$third_bar_status)
        }
      }
    }
  }
  
  return(tower_status)
}

# print(tower_status)
# tower_status = hold(tower_status)
# print(tower_status)
# 
# 
# tower_status = move_right(tower_status)
# print(tower_status)
#


# high = 1, width =1,2,3

plot(c(0,10),c(0,0), 
     type = "l",
     xlim = c(0,8),
     ylim = c(0,5),
     ann = F, bty = "n", xaxt = "n", yaxt ="n")
abline(v=2)
abline(v=4)
abline(v=6)
polygon(c(0,0,1,1), c(0,1,1,0), col = "blue") 

polyred = curve(dnorm(x), xlim = c(-4,4)) 
polygon(polyred$x, polyred$y, col = "red") 



plot_tower_status <- function(tower_status){
  plot()
  
}