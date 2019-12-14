## advent-of-code-2019

https://adventofcode.com/2019

### Day 11

All of the panels are currently black. 
It will paint its current panel black or white. 

Provide 0 if the robot is over a black panel
1 if the robot is over a white panel.

Then, the program will output two values:

First, the color to paint the panel the robot is over: 
- 0 means to paint the panel black, 
- 1 means to paint the panel white.

Second, a value indicating the direction the robot should turn:
 - 0 means it should turn left 90 degrees, 
 - 1 means it should turn right 90 degrees.

After the robot turns, it should always move forward exactly one panel. 

The robot starts facing up.

The robot will continue running for a while like this and halt when it is finished drawing. Do not restart the Intcode computer inside the robot during this process.