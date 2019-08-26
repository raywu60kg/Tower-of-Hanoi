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
  ),
  bar_order = c("first_bar", "second_bar", "third_bar"),
  move_step = 0
)

hold <- function(tower_status){
  if(is.null(tower_status$hold_ring) == FALSE){
    tower_status$hold_ring = c()
  }else{
    bar_index = tower_status$move_step %% 3 + 1
    target_bar_status = tower_status[[paste(tower_status$bar_order[bar_index],"_status",sep="")]]
    if(is.null(target_bar_status) == TRUE){
      return(tower_status)
    }else{
      tower_status$hold_ring = target_bar_status[1]
    }
  }
  return(tower_status)
}

move <- function(tower_status, move_direction){
  if(is.null(tower_status$hold_ring) == TRUE){
    
    # move without hold
    tower_status$move_step = tower_status$move_step + move_direction
    bar_index = tower_status$move_step %% 3 + 1
    tower_status$point_position = tower_status$bar_order[bar_index]
    return(tower_status)
  }else{
    
    # move with hold
    current_ring_weight = tower_status$ring_weight[[tower_status$hold_ring]]
    current_bar_index = tower_status$move_step %% 3 + 1
    current_bar_status = tower_status[[paste(tower_status$bar_order[current_bar_index],"_status",sep="")]]
    target_bar_index = (tower_status$move_step + move_direction)  %% 3 + 1
    target_bar_status = tower_status[[paste(tower_status$bar_order[target_bar_index],"_status",sep="")]]
    
    if(is.null(target_bar_status) == TRUE){
      target_ring_weight = 4
    }else{
      target_ring_weight = tower_status$ring_weight[[target_bar_status[1]]]
    }
    
    # decide can move or not
    if(current_ring_weight > target_ring_weight){
      return(tower_status) 
    }else{
      # move point_position
      tower_status$move_step = tower_status$move_step + move_direction
      bar_index = tower_status$move_step %% 3 + 1
      tower_status$point_position = tower_status$bar_order[bar_index]
      
      # change bar status
      if(length(current_bar_status) == 1){
        tower_status[[paste(tower_status$bar_order[current_bar_index],"_status",sep="")]] = c()
      }else{
        tower_status[[paste(tower_status$bar_order[current_bar_index],"_status",sep="")]] = current_bar_status[-1]
      }
      tower_status[[paste(tower_status$bar_order[target_bar_index],"_status",sep="")]] = c(tower_status$hold_ring, target_bar_status)
      
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
  
  
  
  
  
  