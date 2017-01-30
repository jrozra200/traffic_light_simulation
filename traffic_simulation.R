## SIMULATING TRAFFIC AT THE INTERSECTION OF TWO ROADS WITH EQUAL TRAFFIC ##

## TRAFFIC FLOWS IN THE DIRECTIONS OF THE ARROWS
## THE NAME OF THE ROAD IS WRITTEN AND DIRECTIONAL

#           |   ¦ N |
#           |   ¦ O |
#           |   ¦ R |
#           | | ¦ T |
# __________| ∨ ¦ H |______________
#                   <-<- EAST
# - - - - - -       - - - - - - - -
#  WEST ->->
# ￣￣￣￣￣| S ¦ ∧ |￣￣￣￣￣￣￣
#           | O ¦ | |
#           | U ¦   |
#           | T ¦   |
#           | H ¦   |

## ASSUMING 60 CARS PER HOUR TRAVEL FROM EACH DIRECTION THROUGH THE INTERSECTION
## WE WOULD EXPECT THAT THERE WOULD BE 1 CAR PER MINUTE OR 1 / 60 CARS PER 
## SECOND - 

num_cars_ariving_north <- 0
num_cars_passing_north <- 0
num_cars_in_queue_north <- 0

num_cars_ariving_south <- 0
num_cars_passing_south <- 0
num_cars_in_queue_south <- 0

num_cars_ariving_east <- 0
num_cars_passing_east <- 0
num_cars_in_queue_east <- 0

num_cars_ariving_west <- 0
num_cars_passing_west <- 0
num_cars_in_queue_west <- 0

secs_of_light <- 0

light_north <- "green"
light_south <- "green"
light_east <- "red"
light_west <- "red"

cars_per_second_north <- 30 / 60
cars_per_second_south <- 30 / 60
cars_per_second_east <- 30 / 60
cars_per_second_west <- 30 / 60

z_val_cars_per_sec_north <- qnorm(cars_per_second_north)
z_val_cars_per_sec_south <- qnorm(cars_per_second_south)
z_val_cars_per_sec_east <- qnorm(cars_per_second_east)
z_val_cars_per_sec_west <- qnorm(cars_per_second_west)

for(i in 1:3600){
        samp_north <- rnorm(1)
        samp_south <- rnorm(1)
        samp_east <- rnorm(1)
        samp_west <- rnorm(1)
        
        if(samp_north <= z_val_cars_per_sec_north){ ## CAR ARRIVES
                num_cars_ariving_north <- num_cars_ariving_north + 1
                
                if(light_north == "green"){ ## LIGHT IS GREEN
                        num_cars_passing_north <- num_cars_passing_north + 1
                } else {
                        num_cars_in_queue_north <- num_cars_in_queue_north + 1
                }
        } else { ## NO CAR ARRIVES
                if(light_north == "green") { ## LIGHT IS GREEN
                        if(num_cars_in_queue_north > 0){
                                num_cars_passing_north <- num_cars_passing_north + 1
                                num_cars_in_queue_north <- num_cars_in_queue_north - 1
                        }
                }
        }
        
        if(samp_south <= z_val_cars_per_sec_south){ ## CAR ARRIVES
                num_cars_ariving_south <- num_cars_ariving_south + 1
                
                if(light_south == "green"){ ## LIGHT IS GREEN
                        num_cars_passing_south <- num_cars_passing_south + 1
                } else {
                        num_cars_in_queue_south <- num_cars_in_queue_south + 1
                }
        } else { ## NO CAR ARRIVES
                if(light_south == "green") { ## LIGHT IS GREEN
                        if(num_cars_in_queue_south > 0){
                                num_cars_passing_south <- num_cars_passing_south + 1
                                num_cars_in_queue_south <- num_cars_in_queue_south - 1
                        }
                }
        }
        
        if(samp_east <= z_val_cars_per_sec_east){ ## CAR ARRIVES
                num_cars_ariving_east <- num_cars_ariving_east + 1
                
                if(light_east == "green"){ ## LIGHT IS GREEN
                        num_cars_passing_east <- num_cars_passing_east + 1
                } else {
                        num_cars_in_queue_east <- num_cars_in_queue_east + 1
                }
        } else { ## NO CAR ARRIVES
                if(light_east == "green") { ## LIGHT IS GREEN
                        if(num_cars_in_queue_east > 0){
                                num_cars_passing_east <- num_cars_passing_east + 1
                                num_cars_in_queue_east <- num_cars_in_queue_east - 1
                        }
                }
        }
        
        if(samp_west <= z_val_cars_per_sec_west){ ## CAR ARRIVES
                num_cars_ariving_west <- num_cars_ariving_west + 1
                
                if(light_west == "green"){ ## LIGHT IS GREEN
                        num_cars_passing_west <- num_cars_passing_west + 1
                } else {
                        num_cars_in_queue_west <- num_cars_in_queue_west + 1
                }
        } else { ## NO CAR ARRIVES
                if(light_west == "green") { ## LIGHT IS GREEN
                        if(num_cars_in_queue_west > 0){
                                num_cars_passing_west <- num_cars_passing_west + 1
                                num_cars_in_queue_west <- num_cars_in_queue_west - 1
                        }
                }
        }
        
        #print(paste("Second: ", i, "; Color of Light: ", light, 
        #            "; Number of Cars Arrived: ", num_cars_ariving, 
        #            "; Number of Cars Passed Through: ", num_cars_passing, 
        #            "; Number of Cars in Queue: ", num_cars_in_queue, sep = ""))
        
        secs_of_light <- secs_of_light + 1 ## INCREASE HOW MANY SECONDS LIGHT HAS BEEN AT CURRENT LEVEL
        
        if(secs_of_light >= 30){
                secs_of_light <- 0
                
                light_north <- if(light_north == "green"){
                        "red"
                } else {
                        "green"
                }
                
                light_south <- if(light_south == "green"){
                        "red"
                } else {
                        "green"
                }
                
                light_east <- if(light_east == "green"){
                        "red"
                } else {
                        "green"
                }
                
                light_west <- if(light_west == "green"){
                        "red"
                } else {
                        "green"
                }
                
        }
}

print(paste("NORTH: Number of cars which arrived: ", num_cars_ariving_north, 
            "; Number of cars which passed through: ", num_cars_passing_north, 
            "; Number of cars in queue at the end: ", num_cars_in_queue_north, 
            sep = ""))

print(paste("SOUTH: Number of cars which arrived: ", num_cars_ariving_south, 
            "; Number of cars which passed through: ", num_cars_passing_south, 
            "; Number of cars in queue at the end: ", num_cars_in_queue_south, 
            sep = ""))

print(paste("EAST: Number of cars which arrived: ", num_cars_ariving_east, 
            "; Number of cars which passed through: ", num_cars_passing_east, 
            "; Number of cars in queue at the end: ", num_cars_in_queue_east, 
            sep = ""))
            
print(paste("WEST: Number of cars which arrived: ", num_cars_ariving_west, 
            "; Number of cars which passed through: ", num_cars_passing_west, 
            "; Number of cars in queue at the end: ", num_cars_in_queue_west, 
            sep = ""))