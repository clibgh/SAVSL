# SAVSL
Standalone Vhdl Simulation Library

This library implements a tool usefull for VHDL testbenches.
Short description of the philosophy:

The top-level simulation module must instantiate the SAVSL entity, specifying IO size, source script name and include paths.
All IO's shall be connected to dut(s).

At startup, the VHDL model of SAVSL preproces the script file and sort-of "compile" it into an internal "machine code".
If everything is OK after compilation, scneario is executed.
The machine-code is executed by an engine. 
When needed, sub-scenarios can be started and taken in charge by an additional engine, seen as a parallel task.
process forking is handled dynamically.

low level machine code is able to deal with features such as:
  - direct IO control.
  - Automatic application of a clock waveform on selected IO's.
  - react from several trigger sources:
     - an IO event
     - A delay.
     - An internal "variable" change
     - ...
  - read/write text files.
  - perform atomic operations on data :
     - arithmetic : add, sub, multiply, divide...
     - logic : and, or, xor, ...
     - float number operations
  - use specific internal resources like:
    - storage arrays
    - fifos
    - ...
    - 
The compilation process allow higher level control

The script is started on a first
