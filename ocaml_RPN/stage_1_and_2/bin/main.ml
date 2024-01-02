(* let read_float () = Scanf.bscanf Scanf.Scanning.stdin " %f " (fun x -> x) *)
(* let read_str () = Scanf.bscanf Scanf.Scanning.stdin " %s " (fun x -> x) *)
let pf=Printf.printf
(* let inp=input_line stdin *)

let inp=read_line()

let split str = let rec splitter str index acc prev_index= if index>=String.length str then
        (if prev_index>=String.length str || str.[prev_index]=' ' then acc
        else String.init (String.length str-prev_index) (fun i -> str.[prev_index+i]) :: acc)
else if str.[index]=' ' then (
        if str.[prev_index]=' ' then splitter str (index+1) acc (index+1)
        else splitter str (index+1) (String.init (index-prev_index) (fun i -> str.[prev_index+i]) :: acc) (index+1))
else splitter str (index+1) acc prev_index 
in List.rev (splitter str 0 [] 0)

(* let print () elem= match elem with *)
(* |  Num f->  pf "%f\n" f *)
(* |  Op str-> pf "%s\n" str *)

(* let rec pow a n = if n=0 then 1
else if n=1 then a
else let b=pow a (n/2) in b*b*(if n mod 2=0 then 1 else a) *)

let inp=split inp

(* let () = List.fold_left (fun () -> fun elem -> pf "%s\n" elem) () inp *)

let op num1 c num2 = match c with
| "+" -> num1+.num2
| "-" -> num1-.num2
| "*" -> num1*.num2
| "/" -> num1/.num2
| "^" -> num1**num2
| _-> raise (Failure "Invalid operator")

let eval rpn= let ans = List.fold_left (fun summ -> fun elem -> match elem with
| "+" | "-" | "*" | "/" | "^" -> (match summ with
        |num2::num1::xs -> (try op num1 elem num2 :: xs with _ -> raise (Failure "Invalid RPN expresssion"))
        |_-> raise (Failure "Invalid RPN expression"))
|fl -> try float_of_string fl :: summ with _ -> raise (Failure "Invalid RPN expression")         
) [] rpn 
in match ans with
| [x] -> x
| _ -> raise (Failure "Invalid RPN expression")

let ans = eval inp

let () = pf "%f\n" ans
