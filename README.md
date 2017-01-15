# CS4012 Interpreter Assignment

## Compilation & Running
To compile the project, simply use "stack build". 
To run, type "stack exec debugger". 
The debugger will prompt you for a file to read the program from. 
A sample program for the interpreter can be found in testProg.txt.

The interpreter will present you with a CLI menu.
The available options are as follows:

* n: Run the current statement and advance to the next one.
* p: Revert to the previous state, in terms of both variable values and statement execution. Note that this instruction "unrolls" while loops. In effect, this means that the user does not need to view the comparison operations of the loop every time they try to go backwards.
* o: Display an options menu.
* vars: Display a list of all the variables that currently hold values.
* past: Display a list of all the instructions leading up to our current state. Undone instructions are not displayed.
* inspect <"varname">: Display the current value of the given variable. The variable name must be within quotes.
* history <"varname">: Display the entire history of the given variable up to the current instruction. This includes all steps in the program up to the current one, including states where the variable did not yet have a value.

## Section Completion
I have completed all of the sections of the assignment. 
Details of each particular section can be found below.

### Section 1 - Complete
The project has been made into a 'stack' project for ease of compilation.
Details on the compilation and running process can be found above.

### Section 2 - Complete
The program is a monadic interpreter that allows the user to step through a program.
The program is read from a user-specified file.

### Section 3 - Complete
The interpreter contains an inspect command that can be used to view the contents of a variable at any time during execution.

### Section 4 - Complete
The intepreter records the history of each variable and allows the user to see a full history of the values a variable has held over the course of the execution.

### Section 5 - Complete
The interpreter contains a 'step backwards' command that allows the user to return the interpreter to a previous state and statement. 

### Section 6 - Complete
Static analysis has been added to the project in the form of a preprocessing function that finds any unused variables in the program.
Variables that are assigned to but not used at any point in the program are located and displayed to the user.
One aspect to note about this static analysis is that it does not take into account the branches that will be taken during execution.
For example, if a program's only use of a variable is unreachable, the static analysis will not pick up on it and will report the variable as used.
However, the static analysis is wholly capable of catching unused variables when there is no code at all that makes use of the variable's contents.

## Sample Program
The submission includes a sample program for use with the interpreter.
The program is very simple - it assigns the value '1' to a variable, then enters a while loop and increments the variable until its value is equal to 5.
The program then prints the contents of the variable.
In order to showcase the static analysis, the test program also includes an unused variable.
