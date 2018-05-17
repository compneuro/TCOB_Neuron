README for TCOB
-----------------
TCOB is a constrained-object programming language, a temporal extension of the constrained-object language COB. This document contains instructions to  create and
run TCOB programs in Swi-prolog environment. This is a preliminary version to support paper titled "Temporal Constrained Objects for Modelling Neuronal Dynamics".

Developed by Manjusha Nair, Jinesh Manchan Kannimoola, Bharat Jayaraman, Shyam Diwakar
Amrita Vishwa Vidyapeetam, Amritapuri Campus,Kollam, Kerala, India.
15-May-2018

This folder contains 24 files of the following category 
1. Eleven TCOB Source code files
2. Ten png files showing the output of execution
3. Two compiler files tcob2swi.pl and helper_clpr.pl
4. The current Readme.txt

-----------------------
This software is covered by the Simplified BSD license. 
Copyright (C) 2018 Manjusha Nair, Jinesh Kannimoola, Bharat Jayaraman, Shyam Diwakar
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

---------------------


# Required Softwares
-----------------------
 1. Swi-prolog
  
     o The TCOB compiler translates a TCOB program into a CLP(R) program in the SWI-Prolog environment.In order to work with the compiler, the system must have a 
       latest swi-prolog installation. 
       You can download and install Swi-prolog from http://www.swi-prolog.org/download/stable. 
       The user-manual and documentation of Swi-prolog available on http://www.swi-prolog.org/pldoc/index.htm.

 2. TCOB Compiler
     o The compiler contains following Prolog files
      a) tcob2swi.pl - TCOB compiler source, which translates a TCOB program into a CLP program
     
      b) helper clpr.pl - A collection of TCOB built-in predicates.
# ------------------------------------------------------------------------------------------      

The following section describes steps to compile the accircuit.tcob program. Here ?- is the Prolog prompt for commands.

Step 1 Open SWI-Prolog environment and load the compiler.
     
     $swipl
     ?- [tcob2swi].
Step 2 Compile TCOB using the tcob2swi/2 command. It takes the TCOB  program and driver class constructor as the input and creates the corresponding
     CLP(R) program.
     
     ?- tcob2swi('ac_circuit.tcob', 'samplecircuit()').
     
     It creates a file with a .pl extension and with same name as the TCOB program.
Step 3 Load the compiled code using standard Prolog load command.
     ?- [ac_circuit].
     
     The compiler adds a main class to regulate the simulation time based on user-supplied parameters and it invokes the driver class constructor. The generated
     program has two arguments, the first denotes the list of attributes in the class and second denotes the list of arguments to the constructor. If you are not
     taking any input values, it can be represent by _ character.
     
     ?- main(_,_).
     The result will be 'true' for a successful execution (all the constraints are satisfied), otherwise the result will be 'false'.
 # ---------------------------------------------------------------------------------------------    
     To know the values of variables, we can use the following methods.
     
    * Using write predicates TCOB supports the write/1 and writeln predicates of SWI-Prolog in order to print the value of variable. The user may use
     these predicates in constraints, predicates and constructors of a TCOB class.
     
   Example:
     
     class resistor
     extends component{
     attributes
     real R;
     constraints
     V = I * R;
     writeln(`I);
     constructor resistor(R1)
     { R = R1; }
     }
     
    * Using constructor parameters A constructor can accept any number of parameters which can be used in order to get the values of variables after the
     execution. 
     Consider the following modification in the samplecircuit class in   order to get the value of I from the resistor class.
     
     class samplecircuit {
     attributes
     source AC;
     ....
     constructor samplecircuit(RI) {
     R1 = new resistor(10.0);
     ....
     RI = R1.I;
     }
     }
     
     The compilation steps are as follows.
     ?- tcob2swi('ac_circuit.tcob', 'samplecircuit(RI)').
     ?- [ac_circuit].
     The value of RI after execution can be obtained as follows:
     ?- main(_,[R1]).
     R1 = [0.028415544588302557,
     -0.0026938858952047884,-0.0318564641884161,-0.030040285877712104
     0.0006954638679985727, 0.031661681297034526, 0.03408940847804451
     0.005549458734292934]
true.
  
 
   *dump_to_file(N,V) - Dump the values of variables to a CSV file called     output.csv, where N is the list of variable names and V is the list of     values of variable. Each row in the CSV starts with the name of the     variable. 
     
    eg: dump_to_file(['V','A], [V,A])

  
   *plot_graph(Title,V,XLo,XHi,YLo,YHi,Width,Height,Spacing,TimeFactor)- Plot     the list of value in variable V against time. In X-axis time starts from     XLo and incremented by TimeFactor unit up to XHi. YLo and YHi specifies     the range of Y-axis, Width and Height are the dimensions of chart and     Spacing determines the spacing of labels in X and Y axis
    
    eg: plot_graph('Voltage',V,2, 100,-80,35,720,500,5,0.02);

# Contact 
-------------
Email: manjushanair@am.amrita.edu, shyam@amrita.edu