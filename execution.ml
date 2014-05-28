(* execution.ml *)

open Syntax

type execvalue = 
	  Unit of unit
	| Var of var
	| Int of int
	| Bool of bool
	| Pair of execvalue * execvalue
type execproc = 
	  Nop 
	| Repeat of execproc
	| Parallel of execproc list
	| NewChan of var * execproc
	| IfThenElse of execvalue * execproc * execproc
	| Receive of var * var * execproc
	| Send of var * execvalue * execproc

exception UnknownFV


let expand_par p = 
	let expand p = p
in expand p