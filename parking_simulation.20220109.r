
#library(tidyverse)
library(grid)


`%.%` = function(x,y) paste0(x,y)


carGrob = function(x=0, y=0, wheel_angle=0, width=1, length=2, wheel_to_front=0.3, wheel_to_back=wheel_to_front, wheel_length=0.3, frame_lwd=1, wheel_lwd=3, col="black", default.units="cm"){
	if (!is.unit(x))  x <- unit(x, default.units)
	if (!is.unit(y))  y <- unit(y, default.units)
	if (!is.unit(width))  width <- unit(width, default.units)
	if (!is.unit(length))  length <- unit(length, default.units)
	if (!is.unit(wheel_to_front))  wheel_to_front <- unit(wheel_to_front, default.units)
	if (!is.unit(wheel_to_back))  wheel_to_back <- unit(wheel_to_back, default.units)
	if (!is.unit(wheel_length))  wheel_length <- unit(wheel_length, default.units)

	x1 = x - width*0.5
	x2 = x + width*0.5
	y1 = y - wheel_to_back
	y2 = y1 + length
	y3 = y2 - wheel_to_front
	frameGrob = rectGrob(x=x1 + width*0.5, y=y1 + length*0.5, width=width, height=length, gp=gpar(col=col, lwd=frame_lwd))
	wheel_axis_F = segmentsGrob(x1, y3, x2, y3, gp=gpar(col=col, lwd=frame_lwd))
	wheel_axis_R = segmentsGrob(x1, y, x2, y, gp=gpar(col=col, lwd=frame_lwd))
	wheel_dx = wheel_length*0.5*sinpi(wheel_angle/180)
	wheel_dy = wheel_length*0.5*cospi(wheel_angle/180)
	wheel_FL = segmentsGrob(x1-wheel_dx, y3+wheel_dy, x1+wheel_dx, y3-wheel_dy, gp=gpar(col=col, lwd=wheel_lwd))
	wheel_FR = segmentsGrob(x2-wheel_dx, y3+wheel_dy, x2+wheel_dx, y3-wheel_dy, gp=gpar(col=col, lwd=wheel_lwd))
	wheel_RL = segmentsGrob(x1, y+wheel_length*0.5, x1, y-wheel_length*0.5, gp=gpar(col=col, lwd=wheel_lwd))
	wheel_RR = segmentsGrob(x2, y+wheel_length*0.5, x2, y-wheel_length*0.5, gp=gpar(col=col, lwd=wheel_lwd))
	gTree(children = gList(frameGrob, wheel_axis_F, wheel_axis_R, wheel_FL, wheel_FR, wheel_RL, wheel_RR))
}

drawCarVP = function(x=0, y=0, angle=0, wheel_angle=0, width=1, length=2, wheel_to_front=0.3, wheel_to_back=wheel_to_front, wheel_length=0.3, frame_lwd=1, wheel_lwd=3, col="black", default.units="cm"){
	if (!is.unit(x))  x <- unit(x, default.units)
	if (!is.unit(y))  y <- unit(y, default.units)
	if (!is.unit(width))  width <- unit(width, default.units)
	if (!is.unit(length))  length <- unit(length, default.units)
	if (!is.unit(wheel_to_front))  wheel_to_front <- unit(wheel_to_front, default.units)
	if (!is.unit(wheel_to_back))  wheel_to_back <- unit(wheel_to_back, default.units)
	if (!is.unit(wheel_length))  wheel_length <- unit(wheel_length, default.units)
	
	dy_to_center = length*0.5 - wheel_to_back
	vp_x = x - dy_to_center * sinpi(angle/180)
	vp_y = y + dy_to_center * cospi(angle/180)
	carVP = viewport(x=vp_x, y=vp_y, width=width, height=length, angle=angle)
	carGr = carGrob(x=width*0.5, y=wheel_to_back, wheel_angle=wheel_angle, width=width, length=length, wheel_to_front=wheel_to_front, wheel_to_back=wheel_to_back, wheel_length=wheel_length, frame_lwd=frame_lwd, wheel_lwd=wheel_lwd, col=col, default.units=default.units)
	pushViewport(carVP)
	grid.draw(carGr)
	popViewport()
	invisible(carVP)
}

createCar = function(x=0, y=0, angle=0, wheel_angle=0, wheel_angle_limit=30, width=1, length=2, wheel_to_front=0.3, wheel_to_back=wheel_to_front, wheel_length=0.3, frame_lwd=1, wheel_lwd=3, col="black", default.units="cm"){
	ans = list(x=x, y=y, angle=angle, wheel_angle=wheel_angle, wheel_angle_limit=wheel_angle_limit, width=width, length=length, wheel_to_front=wheel_to_front, wheel_to_back=wheel_to_back, wheel_length=wheel_length, frame_lwd=frame_lwd, wheel_lwd=wheel_lwd, col=col, default.units=default.units)
	class(ans) = "car"
	invisible(ans)
}
drawCar = function(car){
	with(car, drawCarVP(x=x, y=y, angle=angle, wheel_angle=wheel_angle, width=width, length=length, wheel_to_front=wheel_to_front, wheel_to_back=wheel_to_back, wheel_length=wheel_length, frame_lwd=frame_lwd, wheel_lwd=wheel_lwd, col=col, default.units=default.units))
}

carStearWheel = function(car, new_wheel_angle){
	car$wheel_angle = new_wheel_angle
	if(car$wheel_angle > car$wheel_angle_limit){
		car$wheel_angle = car$wheel_angle_limit
	}
	if(car$wheel_angle < -car$wheel_angle_limit){
		car$wheel_angle = -car$wheel_angle_limit
	}
	invisible(car)
}
carStearWheelDelta = function(car, delta_wheel_angle){
	carStearWheel(car, car$wheel_angle + delta_wheel_angle)
}
carMove = function(car, distance){
	if(car$wheel_angle==0){
		car$x = car$x - distance*sinpi(car$angle/180)
		car$y = car$y + distance*cospi(car$angle/180)
	}else{
		radius = (car$length-car$wheel_to_front-car$wheel_to_back) / tanpi(car$wheel_angle/180)
		center_x = car$x - radius*cospi(car$angle/180)
		center_y = car$y - radius*sinpi(car$angle/180)
		angle_distance = distance / radius / pi * 180
		car$angle = car$angle + angle_distance
		car$x = center_x + radius*cospi(car$angle/180)
		car$y = center_y + radius*sinpi(car$angle/180)
	}
	invisible(car)
}

#carDemo = createCar(5, 5)
#carDemo %>% drawCar
#carDemo %>% carMove(5) %>% drawCar
#carDemo %>% carMove(5) %>% carStearWheel(30) %>% carMove(5) %>% drawCar



ToyotaCorolla_scale = c(4630, 1780, 2700)   # length, width, wheel base

ToyotaCorolla_scale = ToyotaCorolla_scale / 1500

restart_simulation = function(userCar_x=5, userCar_y=5, parkedCar_x=6.5, parkedCar_y=c(3,11), parkedCar_angle=0, roadEdge_x=7.3, wheel_length=0.4){
	parkedCar_list = list()
	num_parkedCar = max(c(length(parkedCar_x), length(parkedCar_y)))
	parkedCar_x = rep(parkedCar_x, length.out=num_parkedCar)
	parkedCar_y = rep(parkedCar_y, length.out=num_parkedCar)
	parkedCar_angle = rep(parkedCar_angle, length.out=num_parkedCar)
	for(k in 1:num_parkedCar){
		parkedCar_list[[k]] = createCar(parkedCar_x[k], parkedCar_y[k], angle=parkedCar_angle[k], length=ToyotaCorolla_scale[1], width=ToyotaCorolla_scale[2], wheel_to_front=(ToyotaCorolla_scale[1]-ToyotaCorolla_scale[3])/2, wheel_length=wheel_length)
	}

	userCar = createCar(userCar_x, userCar_y, col="brown", length=ToyotaCorolla_scale[1], width=ToyotaCorolla_scale[2], wheel_to_front=(ToyotaCorolla_scale[1]-ToyotaCorolla_scale[3])/2, wheel_length=wheel_length)
	speed = 0

	redraw = function(){
		grid.newpage()
		for(k in 1:num_parkedCar){
			drawCar(parkedCar_list[[k]])
		}
		drawCar(userCar)
		grid.text("speed=" %.% speed %.% " , wheel_angle=" %.% userCar$wheel_angle %.% " , car_angle=" %.% round(userCar$angle, 3), 0, 0, just=c(0,0))
		for(now_roadEdge_x in roadEdge_x){
			grid.segments(now_roadEdge_x, 0, now_roadEdge_x, 100, default.units="cm", gp=gpar(lwd=2))
		}
	}

	dt = 0.5
	t = 0
	while(TRUE){
		redraw()
		### ref: https://stackoverflow.com/questions/15272916/how-to-wait-for-a-keypress-in-r
		prompt_txt = "Press arrow key to adjust, or any other key to maintain speed, or ESC to end"
		should_exit = FALSE
		getGraphicsEvent(prompt=prompt_txt, consolePrompt=prompt_txt, onKeybd=function(key){
			if(key=="Up"){
				speed <<- round(speed + 0.1, 2)
			}else if(key=="Down"){
				speed <<- round(speed - 0.1, 2)
			}else if(key=="Left"){
				userCar <<- carStearWheelDelta(userCar, + 30/1.5*0.5)
			}else if(key=="Right"){
				userCar <<- carStearWheelDelta(userCar, - 30/1.5*0.5)
			}else if(key=="ctrl-["){   # ESC
				should_exit <<- TRUE
			}else{
#				print(key)
			}
			1
		})
		if(should_exit){
			break
		}
		userCar = carMove(userCar, speed*dt)
		t = t + dt
		Sys.sleep(0.01)
	}
}



simulation_option = 1

if(simulation_option==1){

	## parallel parking
	restart_simulation(userCar_x=5, userCar_y=5, parkedCar_x=6.5, parkedCar_y=c(3,11), parkedCar_angle=0, roadEdge_x=7.3, wheel_length=0.4)

}else if(simulation_option==2){

	## perpendicular parking
	restart_simulation(userCar_x=5, userCar_y=5, parkedCar_x=6.5, parkedCar_y=c(5,8.1), parkedCar_angle=-90, roadEdge_x=c(9.2,2), wheel_length=0.4)

}
### ref: https://www.acko.com/car-guide/how-to-park-a-car-perfectly-easy-guide-to-parallel-parking/

