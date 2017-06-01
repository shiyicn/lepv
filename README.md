# Linear equalities program verification
To recompile : 
$ ocamlbuild -use-ocamlfind 'main.native'
Run the verificator by typing :
$ ./main.native
Then after the sign "Type the txt file name: ", type the file name of the program that you want to verify. Here, we provide a simple example "prog.txt".
To try this example, type : 
$ prog.txt

The verificator raises exception Inv_deduce.DeductionFault when it fails a invariant to invariant deduction. It means that this annotation program is not correct. We can see "Incorrect annotation." . If the verificator finishes without exception, we can see "Correct annotation. " at the end. The verificator raises other exceptions when the txt program doesn't conform to the standard form or somes errors occur during the verification. 