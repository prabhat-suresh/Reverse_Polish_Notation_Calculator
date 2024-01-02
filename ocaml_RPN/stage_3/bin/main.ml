module Polynomial = struct

        type poly = (float*int) list

        let add_poly p1 p2 = let rec add_helper p1 p2 = match (p1,p2) with
        |(((coeff1,pow1)::xs1),((coeff2,pow2)::xs2)) -> 
                        if pow1<pow2 then if coeff1==0.0 then add_helper xs1 p2 else (coeff1,pow1)::add_helper xs1 p2
                        else if pow2<pow1 then if coeff2==0.0 then add_helper p1 xs2 else (coeff2,pow2)::add_helper p1 xs2
                        else if coeff1+.coeff2==0.0 then add_helper xs1 xs2 else (coeff1 +. coeff2,pow1)::add_helper xs1 xs2
        |([],_) -> p2
        |(_,[]) -> p1
                        in add_helper p1 p2

        let mul_poly p1 p2 = let rec mul_helper p1 p2 ans = match p1 with 
        |[]->ans
        |((coeff1,pow1)::xs)->mul_helper xs p2 (List.map (fun (coeff2,pow2) -> (coeff1 *. coeff2, pow1+pow2)) p2 :: ans)
                in List.fold_left (fun summ -> fun elem -> add_poly summ elem) [] (mul_helper p1 p2 [])

        let rec pow_poly p n = if n = 1 then p
                else let phalf=pow_poly p (n/2) in let pn=mul_poly phalf phalf in if n mod 2 = 0 then pn else mul_poly pn p

        let rpn_of_poly (p:poly) = let rpn_of_float coeff = let int_coeff=Float.to_int coeff in (if coeff -. Float.of_int int_coeff = 0.0 then Int.to_string int_coeff else Float.to_string coeff) in let rpn_of_term(coeff,pow)= if pow=0 then rpn_of_float coeff ^ " "
                else "x "^ Int.to_string pow ^ " ^ " ^ rpn_of_float coeff ^ " * " in match p with 
        |[]->""
        |(x::xs)->rpn_of_term x ^ List.fold_left (fun summ -> fun elem -> summ ^ (rpn_of_term elem ^ "+ ")) "" xs

end     
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

let op p1 c p2= match c with
| "+" -> Polynomial.add_poly p1 p2
| "-" -> Polynomial.add_poly p1 (List.map (fun (coeff,pow) -> ((-1.0)*.coeff,pow)) p2)
| "*" -> Polynomial.mul_poly p1 p2
| "^" -> Polynomial.pow_poly p1 (match p2 with
                |[(coeff,0)]->if coeff -. Float.of_int (Float.to_int coeff) = 0.0 && coeff>0.0 then Float.to_int coeff
                                else raise(Failure "invalid expression (power must be a +ve int)")
                |_ -> raise (Failure "invalid expression (power must be a +ve int)"))
| _-> raise (Failure "Invalid operator")

let eval rpn= let ans = List.fold_left (fun summ -> fun elem -> (match elem with
| "+" | "-" | "*" | "^" -> (match summ with
        |p2::p1::xs -> (try op p1 elem p2:: xs with _ -> raise (Failure "Invalid RPN expression 1"))
        |_-> raise (Failure "Invalid RPN expression 2"))
|"x" -> [(1.0,1)]::summ
|fl -> try [(float_of_string fl,0)] :: summ with _ -> raise (Failure "Invalid RPN expression 3")         
)) [] rpn 
in match ans with
        | [x]-> x
        | _ -> raise (Failure "Invalid RPN expression 4")

let ans = eval inp

let ans = Polynomial.rpn_of_poly ans

let ()=pf "%s\n" ans
