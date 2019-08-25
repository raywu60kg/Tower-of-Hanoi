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
          return(tower_status)
        }else{
          tower_status$hold_ring = tower_status$first_bar_status[1]},  
      second_bar = 
        if(is.null(tower_status$second_bar_status) == TRUE){
          return(tower_status)
        }else{
          tower_status$hold_ring = tower_status$second_bar_status[1]},
      third_bar = 
        if(is.null(tower_status$third_bar_status) == TRUE){
          return(tower_status)
        }else{
          tower_status$hold_ring = tower_status$third_bar_status[1]},
    )}
  return(tower_status)
}


move_left <- function(tower_status){
  
  if(tower_status$point_position == "first_bar"){
    return(tower_status)
  }else{
    # move without hold
    if(is.null(tower_status$hold_ring) == TRUE){
      switch(tower_status$point_position,
        second_bar = (tower_status$point_position = "first_bar"),
        third_bar = (tower_status$point_position = "second_bar")
      )
    }else{
      # move with hold
      current_ring_weight = tower_status$ring_weight[[tower_status$hold_ring]]
      
      if(tower_status$point_position == "second_bar"){
        # from second to first bar
        if(is.null(tower_status$first_bar_status) == TRUE){
          target_ring_weight = 4
        }else{
          target_ring_weight = tower_status$ring_weight[[tower_status$first_bar_status[1]]]
        }
        if(current_ring_weight > target_ring_weight){
          return(tower_status) 
        }else{
          tower_status$point_position = "first_bar"
          if(length(tower_status$second_bar_status) == 1){
            tower_status$second_bar_status = c()
          }else{
            tower_status$second_bar_status = tower_status$second_bar_status[-1]
          }
            
          tower_status$first_bar_status = c(tower_status$hold_ring, tower_status$first_bar_status)
        }
      }else{
        # from third to second bar
        
        if(is.null(tower_status$second_bar_status) == TRUE){
          target_ring_weight = 4
        }else{
          target_ring_weight = tower_status$ring_weight[[tower_status$second_bar_status[1]]]
        }
        
        if(current_ring_weight > target_ring_weight){
          return(tower_status) 
        }else{
          tower_status$point_position = "second_bar"
          if(length(tower_status$third_bar_status) == 1){
            tower_status$third_bar_status = c()
          }else{
            tower_status$third_bar_status = tower_status$third_bar_status[-1]
          }
          tower_status$second_bar_status = c(tower_status$hold_ring, tower_status$second_bar_status)
        }
      }
    }
  }
        
  return(tower_status)
}
        

move_right <-function(tower_status){
  
  if(tower_status$point_position == "third_bar"){
    return(tower_status)
  }else{
    # move without hold
    if(is.null(tower_status$hold_ring) == TRUE){
      switch(tower_status$point_position,
             first_bar = (tower_status$point_position = "second_bar"),
             second_bar = (tower_status$point_position = "third_bar")
      )
    }else{
      # move with hold
      current_ring_weight = tower_status$ring_weight[[tower_status$hold_ring]]
      
      if(tower_status$point_position == "first_bar"){
        # move from first to second
        if(is.null(tower_status$second_bar_status) == TRUE){
          target_ring_weight = 4
        }else{
          target_ring_weight = tower_status$ring_weight[[tower_status$second_bar_status[1]]]
        }
        if(current_ring_weight > target_ring_weight){
          return(tower_status) 
        }else{
          tower_status$point_position = "second_bar"
          if(length(tower_status$second_bar_status) == 1){
            tower_status$first_bar_status = c()
          }else{
            tower_status$first_bar_status = tower_status$first_bar_status[-1]
          }
          tower_status$second_bar_status = c(tower_status$hold_ring, tower_status$second_bar_status)
          
        }
      }else{
        # move from second to third
        if(is.null(tower_status$third_bar_status) == TRUE){
          target_ring_weight = 4
        }else{
          target_ring_weight = tower_status$ring_weight[[tower_status$third_bar_status[1]]]
        }
        if(current_ring_weight > target_ring_weight){
          return(tower_status) 
        }else{
          tower_status$point_position = "third_bar"
          if(length(tower_status$second_bar_status) == 1){
            tower_status$second_bar_status = c()
          }else{
            tower_status$second_bar_status = tower_status$second_bar_status[-1]
          }
          tower_status$third_bar_status = c(tower_status$hold_ring, tower_status$third_bar_status)
        }
      }
    }
  }
  
  return(tower_status)
}


plot_tower_status <- function(tower_status){
  
  # base plot
  plot(c(0,10),c(0,0), 
       type = "l",
       xlim = c(0,10),
       ylim = c(0,5),
       ann = F, 
       bty = "n", 
       xaxt = "n", 
       yaxt ="n")
  
  abline(v=2)
  abline(v=5)
  abline(v=8)
  
  # first bar 
  for(bar_status in c('first_bar_status', 'second_bar_status', 'third_bar_status')){
    if(is.null(tower_status[[bar_status]]) == FALSE){
    
      red_on_top = 0
      if((length(grep(tower_status$point_position, bar_status)) == 1) && (is.null(tower_status$hold_ring) == FALSE)){
        red_on_top = 1
      }
      
      ring_index = 1
      switch (bar_status,
        first_bar_status = (center_point_x = 2),
        second_bar_status = (center_point_x = 5),
        third_bar_status = (center_point_x = 8)
      )
      center_point_y = 0.5
      for(ring in rev(tower_status[[bar_status]])){
        switch(ring,
               big_ring = (ring_width = 3),
               medium_ring = (ring_width = 2),
               small_ring = (ring_width = 1)
        )
        x_info = c(center_point_x-ring_width/2,
                   center_point_x-ring_width/2,
                   center_point_x+ring_width/2,
                   center_point_x+ring_width/2
        )
        y_info = c(center_point_y-0.5,
                   center_point_y+0.5,
                   center_point_y+0.5,
                   center_point_y-0.5
        )
        if(ring_index == length(tower_status[[bar_status]]) && red_on_top == 1){
          polygon(x_info, y_info, col = "red")
        }else{
          polygon(x_info, y_info, col = "blue")
        }
        center_point_y = center_point_y + 1
        ring_index = ring_index + 1 
      }
    
    }
  }

  
  # point 
  switch(tower_status$point_position,
    first_bar = points(2,4,pch=8,col="red",cex=3),
    second_bar = points(5,4,pch=8,col="red",cex=3),
    third_bar = points(8,4,pch=8,col="red",cex=3)
  )
}
  
  
  
  
  
  