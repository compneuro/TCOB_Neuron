README for C programs
-----------------
 
This is a preliminary readme of the C programs provided with the paper titled "Temporal Constrained Objects for Modelling Neuronal Dynamics".

Developed by Manjusha Nair, Jinesh Manchan Kannimoola, Bharat Jayaraman, Shyam Diwakar
Amrita Vishwa Vidyapeetam, Amritapuri Campus,Kollam, Kerala, India.
15-May-2018
-----------------------
This software is covered by the Simplified BSD license. 
Copyright (C) 2018 Manjusha Nair, Jinesh Kannimoola, Bharat Jayaraman, Shyam Diwakar
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

---------------------
The folder contains 6 files (three .h files and three corresponding .c files)

1. HH.h and HHmain.c  - To model Hodgkin Huxley type neurons
2. Adex.h and Adexmain.h - To model Adapative Exponential Leaky Integrate and    fire type neurons
3. AdexWithSynapse.h and AdexWithSynapsemain.h - To model adex neurons receiving, AMPA, NMDA and GABA synaptic inputs.

# Required Software 
-----------------------
Any C compiler

#Execution Commands in Linux
--------------------------
1.Develop C programs  using any standard text editor. Save the file with .c extension.  E.g., test.c

2.Open terminal and compile the code using the command
gcc test.c

3. The above command stores the executable in a.out. To run it,use the command
 ./a.out

#Contact 
-------------
Email: manjushanair@am.amrita.edu, shyam@amrita.edu