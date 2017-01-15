# CS4012 Interpreter Assignment

## Part 1 - Compilation & Running
To compile the project, simply use "stack build". 
To run, type "stack exec debugger". 
The debugger will prompt you for a file to read the program from. 
A sample program for the interpreter can be found in testProg.txt.

The interpreter will present you with a CLI menu.
The available options are as follows:

* n: Run the current statement and advance to the next one.
* p: Revert to the previous state, in terms of both variable values and statement execution.
* o: Display an options menu.
* vars: Display a list of all the variables that currently hold values.
* past: Display a list of all the instructions leading up to our current state. Undone instructions are not displayed.
* inspect <"varname">: Display the current value of the given variable. The variable name must be within quotes.
* history <"varname">: Display the entire history of the given variable up to the current instruction. This includes all steps in the program up to the current one, including states where the variable did not yet have a value.


