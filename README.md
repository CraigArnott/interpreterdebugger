# CS4012 Interpreter Assignment

## Part 1 - Compilation & Running
To compile the project, simply use "stack build". 
To run, type "stack exec debugger". 
The debugger will prompt you for a file to read the program from. 
A sample program for the interpreter can be found in testProg.txt.

The interpreter will present you with a CLI menu.
The available options are as follows:

1) n: Run the current statement and advance to the next one.
2) p: Revert to the previous state, in terms of both variable values and statement execution.
3) o: Display an options menu.
4) vars: Display a list of all the variables that currently hold values.
5) past: Display a list of all the instructions leading up to our current state. Undone instructions are not displayed.
6) inspect <"varname">: Display the current value of the given variable. The variable name must be within quotes.
7) history <"varname">: Display the entire history of the given variable up to the current instruction. This includes all steps in the program up to the current one, including states where the variable did not yet have a value.


