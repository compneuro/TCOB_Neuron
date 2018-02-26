README for TCOB
-----------------
 
TCOB is a constrained-object programming language, a temporal extension of the constrained-object language COB. This document contains instructions to  create and run TCOB programs in Swi-prolog environment. In this implementation, we show how modeling neural and microcircuit network dynamics can be achieved using TCOB. 

Developed by Manjusha Nair, Jinesh Manchan Kannimoola, Shyam Diwakar
Amrita Vishwa Vidyapeetham (University), Kollam, Kerala, India.
and by Bharat Jayaraman
State University at Buffalo, NY, USA.
Last updated 26-February-2018

More details will be made available. This is a preliminary version of the code for results in the paper titled "A Temporal Constrained Object based Declarative Programming Paradigm for Modelling Neuronal Dynamics" (manuscript submitted).

# Required Softwares
-----------------------
 1. Swi-prolog
  
     o The TCOB compiler builts up on the SWI-prolog implementation. In order to work with the compiler, the system must have a latest swi-prolog installation. 
       You can download and install Swi-prolog from http://www.swi-prolog.org/download/stable. 
       The user-manual and documentation of Swi-prolog available on http://www.swi-prolog.org/pldoc/index.htm.

 2. TCOB Compiler
     o The TCOB compiler has four files 
         * dcob2cob.pl,cob2swi.pl: TCOB compiler code, which convert TCOB program into CLP program
         * main.pl: Used to load compiler code to prolog environment
         * helper_clpr.pl : Collection of TCOB built-in predicates


     o Use the Compiler provided with the sample code here and move the four files(main.pl,dcob2cob.pl,cob2swi.pl,helper_clpr.pl) to your prolog working directory
 

#Execution Commands
--------------------------
1.Develop TCOB models  by writing textual TCOB code using any standard text editor. Save the file with .tcob extension.  E.g., test.tcob

2.Open swi prolog environment 

3.Load the compiler code using following command.


    ?- [main]



4. Compile TCOB using the following command. TCOB is then translated to CLP(R) code by the TCob compiler

    ?- tcob2swi('test.tcob').

    It creates a prolog file with same name as the TCOB program. E.g., test.pl

5.Load compiled code(prolog file) to current prolog environment using standard prolog load command.

    ?- [test]


6. invoke the program using the driver(main) class predicates in the CLP program. Call the constructor of TCOB driver class using the following command.


    ?- test(_,_)

            
    It has two arguments, first argument represents the list of attributes in the class and second argument denotes the list of arguments to the constructor. If no     inputs are taken  from the user,it can be represent by an _ character 

 7. The simulation output is saved in output.csv

 

#Contact 
-------------
Email: manjushanair@am.amrita.edu, shyam@amrita.edu