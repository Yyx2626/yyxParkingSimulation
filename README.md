# yyxParkingSimulation
Parking Simulation

Theoretical simulation for learning parking (such as parallel or perpendicular parking)

Implemented by R with grid graphics system

### Prerequisite

R console in Windows and R package {grid}

### Usage

Just load and run parking_simulation.20220109.r in R by
dragging the R script file into R console, or by executing
```source("parking_simulation.20220109.r")``` in R console.

You may manually change the code ```simulation_option = 1``` to ```simulation_option = 2``` for perpendicular parking instead of parallel parking.

Or you may also manually execute ```restart_simulation()``` with different parameters, or even manually change the initial setting in the code of ```restart_simulation()``` (before and defining redraw), for example, change colors, if you like.

### Demo screenshots

- Parallel parking (```simulation_option = 1```):

<p fload="left">
<img src="img/screenshot_1.png" alt="parallel parking" width="33%" />
<img src="img/screenshot_1_2.png" alt="parallel parking_2" width="33%" />
<img src="img/screenshot_1_3.png" alt="parallel parking_3" width="33%" />
<img src="img/screenshot_1_4.png" alt="parallel parking_4" width="33%" />
<img src="img/screenshot_1_5.png" alt="parallel parking_5" width="33%" />
<img src="img/screenshot_1_6.png" alt="parallel parking_6" width="33%" />
</p>

- Perpendicular parking (```simulation_option = 2```):

<p fload="left">
<img src="img/screenshot_2.png" alt="perpendicular parking" width="33%" />
<img src="img/screenshot_2_2.png" alt="perpendicular parking_2" width="33%" />
<img src="img/screenshot_2_3.png" alt="perpendicular parking_3" width="33%" />
<img src="img/screenshot_2_4.png" alt="perpendicular parking_4" width="33%" />
<img src="img/screenshot_2_5.png" alt="perpendicular parking_5" width="33%" />
</p>

