type stackValue = BOOL of bool | INT of int | ERROR of string | UNIT of string | STRING of string | NAME of string | CLOSURE of (stackValue * (string list) * ((stackValue * stackValue) list) * stackValue) | FUNF | FUNIO

type command = PUSH of stackValue | POP | ADD | SUB | MUL | DIV | REM | NEG | SWAP | TOSTRING | PRINTLN | QUIT | CAT | AND | OR | NOT | EQUAL | LESSTHAN | BIND | IF | LET | END | FUN of (stackValue * stackValue) | FUNEND | RETURN | CALL | CALLEND | INOUTFUN of (stackValue * stackValue)

let interpreter (input, output) =

    (*opens input file*)
    let ic = open_in input
    in

    (*opens output file*)
    let oc = open_out output
    in

    (*reads every line and returns a string list, each element is a line*)
    let rec loop_read acc =
        try
            let l = String.trim(input_line ic) in loop_read (l::acc)
        with
            | End_of_file -> List.rev acc
    in
    
    (*helper function: takes a string, writes it to the output file, followed by a newline*)
    let file_write str = Printf.fprintf oc "%s\n" str in
    
    (*converts file to string list*)
    let strList = loop_read []
    in

    (*checks if a string is an int*)
    let check_str_is_int (s: string) : bool = 
        try int_of_string s |> ignore; true
        with Failure _ -> false
    in

    (*checks if a string is a NAME - first char is a letter or '_' on ASCII table*)
    let check_name (s: string) : bool =
        let c = String.get s 0 in
        if (Char.code c >= 65 && Char.code c <= 90) || (Char.code c >= 97 && Char.code c <= 122 || Char.code c == 95)
            then true
        else 
            false
    in

    (*converts a string into a stackValue *)(*uses functions check_str_is_int, check_name*)
    let str2sv (s: string) : stackValue =
        match s with
        | ":true:" ->BOOL(true)
        | ":false:" -> BOOL(false)
        | ":error:" -> ERROR(":error:")
        | ":unit:" -> UNIT(":unit:")
        | _ ->  (if String.get s 0 == '"'
                    then STRING(String.sub s 1 ((String.rindex s '"') - 1))
                else if check_str_is_int s
                    then INT(int_of_string s)
                else if check_name s
                    then NAME(s)
                else
                    ERROR(":error:")
                )
    in

    (*helper function for CLOSURE*)
    (*converts a stackValue into a string*)
    let rec sv2str (sv: stackValue) : string =
        match sv with
        | BOOL(true) -> ":true:"
        | BOOL(false) -> ":false:"
        | ERROR(x) -> x
        | UNIT(x) -> x
        | STRING(x) -> "\"" ^ x ^ "\""
        | INT(x) -> string_of_int x
        | NAME(x) -> x
        | CLOSURE(x1, x2, x3, x4) -> sv2str x1
        | FUNF -> "fun"
        | FUNIO -> "inOutFun"
    in

    (*converts a string into a command*)(*uses functions: str2sv*)
    let str2com (s: string) : command =
        match s with
        | "add" -> ADD
        | "sub" -> SUB
        | "mul" -> MUL
        | "div" -> DIV
        | "pop" -> POP
        | "rem" -> REM
        | "neg" -> NEG
        | "swap" -> SWAP
        | "toString" -> TOSTRING
        | "println" -> PRINTLN
        | "quit" -> QUIT
        | "cat" -> CAT
        | "and" -> AND
        | "or" -> OR
        | "not" -> NOT
        | "equal" -> EQUAL
        | "lessThan" -> LESSTHAN
        | "bind" -> BIND
        | "if" -> IF
        | "let" -> LET
        | "end" -> END
        | "funEnd" -> FUNEND
        | "return" -> RETURN
        | "call" -> CALL
        | "callend" -> CALLEND
        | _ -> (match String.split_on_char ' ' s with                           (*get a string list, each element contains a word*)
                | [] -> ADD                                                     (*This doesn't actually do anything as we will never have an empty list*)
                | x::xs -> (match x with                                        (*checks the first element in list*)
                            | "push" -> PUSH (str2sv (String.concat " " xs))
                            | "fun" -> (match xs with
                                        | [] -> ADD                             (*This doesn't actually do anything as we will never have an empty list*)
                                        | x1::x2::xss -> FUN (str2sv x1, str2sv x2)
                                        )
                            | "inOutFun" -> (match xs with
                                        | [] -> ADD                             (*This doesn't actually do anything as we will never have an empty list*)
                                        | x1::x2::xss -> INOUTFUN (str2sv x1, str2sv x2)
                                        )
                            )
                )
    in

    (*helper function for CLOSURE*)
    (*converts a command into a string*)(*uses functions: sv2str*)
    let com2str (c: command) : string =
        match c with
        | PUSH(x) -> "push " ^ (sv2str x)
        | ADD -> "add"
        | SUB -> "sub"
        | MUL -> "mul"
        | DIV -> "div"
        | POP -> "pop"
        | REM -> "rem"
        | NEG -> "neg"
        | SWAP -> "swap"
        | TOSTRING -> "toString"
        | PRINTLN -> "println"
        | QUIT -> "quit"
        | CAT -> "cat"
        | AND -> "and"
        | OR -> "or"
        | NOT -> "not"
        | EQUAL -> "equal"
        | LESSTHAN -> "lessThan"
        | BIND -> "bind"
        | IF -> "if"
        | LET -> "let"
        | END -> "end"
        | FUN(x1, x2) -> "fun " ^ (sv2str x1) ^ " " ^ (sv2str x2)
        | FUNEND -> "funEnd"
        | RETURN -> "return"
        | CALL -> "call"
        | CALLEND -> "callend"
        | INOUTFUN(x1, x2) -> "inOutFun " ^ (sv2str x1) ^ " " ^ (sv2str x2)
    in

    (*adds a key-value pair (NAME, value) into the current scope of an environment *)
    let bind_value (value: stackValue) (name: stackValue) (env: (stackValue * stackValue) list list): (stackValue * stackValue) list list =
        match env with
        | [] -> ((name, value)::[])::env
        | e1::envtail -> ((name, value)::e1)::envtail
    in

    (*gets the value of a NAME from an environment: first searches current scope, then the scope's ancestors*)
    let rec fetch (name: stackValue) (env: (stackValue * stackValue) list list): stackValue =
        match env with
        | [] -> ERROR(":error:")
        | e1::envtail -> (match e1 with
                            | [] -> fetch name envtail
                            | (n1, v1)::e1tail -> (if (compare name n1) == 0
                                                        then v1
                                                    else
                                                        fetch name ((e1tail)::envtail)
                                                    )
                            )
    in

    (*gets the value of a NAME from an environment the value cannot be another name: first searches current scope, then the scope's ancestors*)
    let rec fetchterminal (name: stackValue) (env: (stackValue * stackValue) list list): stackValue =
        match env with
        | [] -> ERROR(":error:")
        | e1::envtail -> (match e1 with
                            | [] -> fetchterminal name envtail
                            | (n1, v1)::e1tail -> (if (compare name n1) == 0
                                                        then (match v1 with
                                                                | NAME(x1) -> fetchterminal v1 ((e1tail)::envtail)
                                                                | _ -> v1
                                                                )
                                                    else
                                                        fetchterminal name ((e1tail)::envtail)
                                                    )
                            )
    in

    (*gets the name of a function*)
    let rec fetchfun (name: stackValue) (env: (stackValue * stackValue) list list): stackValue =
        match env with
        | [] -> ERROR(":error:")
        | e1::envtail -> (match e1 with
                            | [] -> fetchfun name envtail
                            | (n1, v1)::e1tail -> (if (compare name n1) == 0
                                                        then (match v1 with
                                                            | NAME(x1) -> fetchfun v1 ((e1tail)::envtail)
                                                            | CLOSURE(x1) -> n1
                                                            | _ -> n1
                                                            )
                                                        else
                                                        fetchfun name ((e1tail)::envtail)
                                                    )
                            )
    in

    (*gets value of a NAME from a single environment scope*)
    let rec fetchscope (name: stackValue) (env: (stackValue * stackValue) list): stackValue =
        match env with
        | [] -> ERROR(":error:")
        | (n1, v1)::envtail -> (if (compare name n1) == 0
                                    then v1
                                else
                                    fetchscope name envtail
                                )
    in

    (*get the body of a function*)
    let rec funbody (commlist: command list) (numsubfun: int): command list =
        match commlist with
        | [] -> []
        | FUN(x1, x2)::commlisttail -> FUN(x1, x2)::(funbody commlisttail (numsubfun + 1))
        | INOUTFUN(x1, x2)::commlisttail -> INOUTFUN(x1, x2)::(funbody commlisttail (numsubfun + 1))
        | FUNEND::commlisttail -> (if numsubfun == 0
                                        then []
                                    else
                                        FUNEND::(funbody commlisttail (numsubfun - 1))
                                    )
        | com1::commlisttail -> com1::(funbody commlisttail numsubfun)
    in

    (*get the rest of the command list after the body of a function [FUNEND; ...]*)
    let rec funbodytail (commlist: command list) (numsubfun: int): command list =
        match commlist with
        | [] -> []
        | FUN(x1, x2)::commlisttail -> (funbodytail commlisttail (numsubfun + 1))
        | INOUTFUN(x1, x2)::commlisttail -> (funbodytail commlisttail (numsubfun + 1))
        | FUNEND::commlisttail -> (if numsubfun == 0
                                        then commlist
                                    else
                                        (funbodytail commlisttail (numsubfun - 1))
                                    )
        | com1::commlisttail -> (funbodytail commlisttail numsubfun)
    in

    (*takes in an env list list and returns one big env list*)
    let rec listlist_to_List (env: (stackValue * stackValue) list list) (biglist: (stackValue * stackValue) list): (stackValue * stackValue) list =
        match env with
        | [] -> []
        | env1::envtail -> (match env1 with
                            | [] -> (listlist_to_List envtail biglist)
                            | e1::env1tail -> e1::(listlist_to_List (env1tail::envtail) biglist)
                            )
    in


    (*Generic function that takes a function and a list, and applies that function to every element in the list*)
    let rec map f list =
        match list with
        | [] -> []
        | x::xs -> (f x)::(map f xs)
    in

    (*converts the string list to a command list*)
    let comList: command list = map str2com strList
    in

    (*creates the stack of stacks for the command list*)
    let comListList: command list list = comList::[]
    in

    let rec elements listlist =
        match listlist with
        | [] -> " empty"
        | list1::listlisttail -> (match list1 with
                            | [] -> "|"
                            | e1::list1tail -> (sv2str e1) ^ ":" ^ (elements (list1tail::listlisttail))
                            )
    in


    (*takes a command list, processes it, returns the completed program*)
    let rec processor (cll: command list list) (stackofstacks: stackValue list list) (env: (stackValue * stackValue) list list) (funstack: stackValue list): unit =
        match cll with
        | [] -> print_string "end of program" (*this print statement is for testing purposes*)
        | comlist1::comliststail -> (match comlist1 with
                                    | [] -> print_string "end of program" (*this print statement is for testing purposes*)
                                    | com1::comlist1tail -> (*print_string ((com2str com1) ^ ":  ");
                                                            (match stackofstacks with
                                                                | [] -> print_string "empty\n"
                                                                | stack::sostail -> (match stack with
                                                                                        | [] -> print_string "emptystack\n"
                                                                                        | s1::stacktail -> print_string ((sv2str s1) ^ "\n")
                                                                                        )
                                                                );*)
                                                                
                                                                (*print_string ((elements stackofstacks) ^ "\n");*)
                                                                

                                                            (match com1 with
                                                            | PUSH (x) -> (match stackofstacks with
                                                                            | [] -> processor ((comlist1tail)::comliststail) ((x::[])::stackofstacks) env funstack  
                                                                            | stack::sostail -> processor ((comlist1tail)::comliststail) ((x::stack)::sostail) env funstack 
                                                                            )
                                                            | POP -> (match stackofstacks with
                                                                        | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::[])::stackofstacks) env funstack 
                                                                        | stack::sostail -> (match stack with
                                                                                                | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                | s1::stacktail -> processor ((comlist1tail)::comliststail) ((stacktail)::sostail) env funstack 
                                                                                        )
                                                                        )
                                                            | ADD -> (match stackofstacks with
                                                                        | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::[])::stackofstacks) env funstack 
                                                                        | stack::sostail -> (match stack with
                                                                                                | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                | INT(x1)::INT(x2)::stacktail -> processor ((comlist1tail)::comliststail) ((INT(x2 + x1)::stacktail)::sostail) env funstack 
                                                                                                | NAME(x1)::INT(x2)::stacktail -> (match (fetchterminal (NAME(x1)) env) with
                                                                                                                                    | INT(i1) -> processor ((comlist1tail)::comliststail) ((INT(x2 + i1)::stacktail)::sostail) env funstack 
                                                                                                                                    | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                    )
                                                                                                | INT(x1)::NAME(x2)::stacktail -> (match (fetchterminal (NAME(x2)) env) with
                                                                                                                                    | INT(i2) -> processor ((comlist1tail)::comliststail) ((INT(i2 + x1)::stacktail)::sostail) env funstack 
                                                                                                                                    | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                    )
                                                                                                | NAME(x1)::NAME(x2)::stacktail -> (match ((fetchterminal (NAME(x1)) env), (fetchterminal (NAME(x2)) env)) with
                                                                                                                                    | (INT(i1), INT(i2)) -> processor ((comlist1tail)::comliststail) ((INT(i2 + i1)::stacktail)::sostail) env funstack 
                                                                                                                                    | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                    )
                                                                                                | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                            )
                                                                        )
                                                            | SUB -> (match stackofstacks with
                                                                        | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::[])::stackofstacks) env funstack 
                                                                        | stack::sostail -> (match stack with
                                                                                                | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                | INT(x1)::INT(x2)::stacktail -> processor ((comlist1tail)::comliststail) ((INT(x2 - x1)::stacktail)::sostail) env funstack 
                                                                                                | NAME(x1)::INT(x2)::stacktail -> (match (fetchterminal (NAME(x1)) env) with
                                                                                                                                    | INT(i1) -> processor ((comlist1tail)::comliststail) ((INT(x2 - i1)::stacktail)::sostail) env funstack 
                                                                                                                                    | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                    )
                                                                                                | INT(x1)::NAME(x2)::stacktail -> (match (fetchterminal (NAME(x2)) env) with
                                                                                                                                    | INT(i2) -> processor ((comlist1tail)::comliststail) ((INT(i2 - x1)::stacktail)::sostail) env funstack 
                                                                                                                                    | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                    )
                                                                                                | NAME(x1)::NAME(x2)::stacktail -> (match ((fetchterminal (NAME(x1)) env), (fetchterminal (NAME(x2)) env)) with
                                                                                                                                    | (INT(i1), INT(i2)) -> processor ((comlist1tail)::comliststail) ((INT(i2 - i1)::stacktail)::sostail) env funstack 
                                                                                                                                    | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                    )
                                                                                                | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                            )
                                                                        )
                                                            | MUL -> (match stackofstacks with
                                                                        | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::[])::stackofstacks) env funstack 
                                                                        | stack::sostail -> (match stack with
                                                                                                | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                | INT(x1)::INT(x2)::stacktail -> processor ((comlist1tail)::comliststail) ((INT(x2 * x1)::stacktail)::sostail) env funstack 
                                                                                                | NAME(x1)::INT(x2)::stacktail -> (match (fetchterminal (NAME(x1)) env) with
                                                                                                                                    | INT(i1) -> processor ((comlist1tail)::comliststail) ((INT(x2 * i1)::stacktail)::sostail) env funstack 
                                                                                                                                    | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                    )
                                                                                                | INT(x1)::NAME(x2)::stacktail -> (match (fetchterminal (NAME(x2)) env) with
                                                                                                                                    | INT(i2) -> processor ((comlist1tail)::comliststail) ((INT(i2 * x1)::stacktail)::sostail) env funstack 
                                                                                                                                    | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                    )
                                                                                                | NAME(x1)::NAME(x2)::stacktail -> (match ((fetchterminal (NAME(x1)) env), (fetchterminal (NAME(x2)) env)) with
                                                                                                                                    | (INT(i1), INT(i2)) -> processor ((comlist1tail)::comliststail) ((INT(i2 * i1)::stacktail)::sostail) env funstack 
                                                                                                                                    | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                    )
                                                                                                | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                            )
                                                                        )
                                                            | DIV -> (match stackofstacks with
                                                                        | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::[])::stackofstacks) env funstack 
                                                                        | stack::sostail -> (match stack with
                                                                                                | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                | INT(x1)::INT(x2)::stacktail -> (if x1 == 0
                                                                                                                                        then processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                    else
                                                                                                                                        processor ((comlist1tail)::comliststail) ((INT(x2 / x1)::stacktail)::sostail) env funstack 
                                                                                                                                    )
                                                                                                | NAME(x1)::INT(x2)::stacktail -> (match (fetchterminal (NAME(x1)) env) with
                                                                                                                                    | INT(i1) -> (if i1 == 0
                                                                                                                                                        then processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                                    else
                                                                                                                                                        processor ((comlist1tail)::comliststail) ((INT(x2 / i1)::stacktail)::sostail) env funstack 
                                                                                                                                                    )
                                                                                                                                    | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                    )
                                                                                                | INT(x1)::NAME(x2)::stacktail -> (match (fetchterminal (NAME(x2)) env) with
                                                                                                                                    | INT(i2) -> (if x1 == 0
                                                                                                                                                        then processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                                    else
                                                                                                                                                        processor ((comlist1tail)::comliststail) ((INT(i2 / x1)::stacktail)::sostail) env funstack 
                                                                                                                                                    )
                                                                                                                                    | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                    )
                                                                                                | NAME(x1)::NAME(x2)::stacktail -> (match ((fetchterminal (NAME(x1)) env), (fetchterminal (NAME(x2)) env)) with
                                                                                                                                    | (INT(i1), INT(i2)) -> (if i1 == 0
                                                                                                                                                                    then processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                                                else
                                                                                                                                                                    processor ((comlist1tail)::comliststail) ((INT(i2 / i1)::stacktail)::sostail) env funstack 
                                                                                                                                                            )
                                                                                                                                    | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                    )
                                                                                                | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                            )
                                                                        )
                                                            | REM -> (match stackofstacks with
                                                                        | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::[])::stackofstacks) env funstack 
                                                                        | stack::sostail -> (match stack with
                                                                                                | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                | INT(x1)::INT(x2)::stacktail -> (if x1 == 0
                                                                                                                                        then processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                    else
                                                                                                                                        processor ((comlist1tail)::comliststail) ((INT(x2 mod x1)::stacktail)::sostail) env funstack 
                                                                                                                                    )
                                                                                                | NAME(x1)::INT(x2)::stacktail -> (match (fetchterminal (NAME(x1)) env) with
                                                                                                                                    | INT(i1) -> (if i1 == 0
                                                                                                                                                        then processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                                    else
                                                                                                                                                        processor ((comlist1tail)::comliststail) ((INT(x2 mod i1)::stacktail)::sostail) env funstack 
                                                                                                                                                    )
                                                                                                                                    | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                    )
                                                                                                | INT(x1)::NAME(x2)::stacktail -> (match (fetchterminal (NAME(x2)) env) with
                                                                                                                                    | INT(i2) -> (if x1 == 0
                                                                                                                                                        then processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                                    else
                                                                                                                                                        processor ((comlist1tail)::comliststail) ((INT(i2 mod x1)::stacktail)::sostail) env funstack 
                                                                                                                                                    )
                                                                                                                                    | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                    )
                                                                                                | NAME(x1)::NAME(x2)::stacktail -> (match ((fetchterminal (NAME(x1)) env), (fetchterminal (NAME(x2)) env)) with
                                                                                                                                    | (INT(i1), INT(i2)) -> (if i1 == 0
                                                                                                                                                                    then processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                                                else
                                                                                                                                                                    processor ((comlist1tail)::comliststail) ((INT(i2 mod i1)::stacktail)::sostail) env funstack 
                                                                                                                                                            )
                                                                                                                                    | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                    )
                                                                                                | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                            )
                                                                        )
                                                            | NEG -> (match stackofstacks with
                                                                        | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::[])::stackofstacks) env funstack 
                                                                        | stack::sostail -> (match stack with
                                                                                                | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                | INT(x1)::stacktail -> processor ((comlist1tail)::comliststail) ((INT(x1 - (x1 + x1))::stacktail)::sostail) env funstack 
                                                                                                | NAME(x1)::stacktail -> (match (fetchterminal (NAME(x1)) env) with
                                                                                                                            | INT(i1) -> processor ((comlist1tail)::comliststail) ((INT(i1 - (i1 + i1))::stacktail)::sostail) env funstack 
                                                                                                                            | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                            )
                                                                                                | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                            )
                                                                        )
                                                            | SWAP -> (match stackofstacks with
                                                                        | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::[])::stackofstacks) env funstack 
                                                                        | stack::sostail -> (match stack with
                                                                                                | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                | s1::s2::stacktail -> processor ((comlist1tail)::comliststail) ((s2::s1::stacktail)::sostail) env funstack 
                                                                                                | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                            )
                                                                        )
                                                            | TOSTRING -> (match stackofstacks with
                                                                        | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::[])::stackofstacks) env funstack 
                                                                        | stack::sostail -> (match stack with
                                                                                                | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                | INT(x1)::stacktail -> processor ((comlist1tail)::comliststail) ((STRING(string_of_int x1)::stacktail)::sostail) env funstack 
                                                                                                | BOOL(x1)::stacktail -> processor ((comlist1tail)::comliststail) ((STRING(":" ^(string_of_bool x1) ^ ":")::stacktail)::sostail) env funstack 
                                                                                                | ERROR(x1)::stacktail -> processor ((comlist1tail)::comliststail) ((STRING(x1)::stacktail)::sostail) env funstack 
                                                                                                | UNIT(x1)::stacktail -> processor ((comlist1tail)::comliststail) ((STRING(x1)::stacktail)::sostail) env funstack 
                                                                                                | STRING(x1)::stacktail -> processor ((comlist1tail)::comliststail) ((STRING(x1)::stacktail)::sostail) env funstack 
                                                                                                | NAME(x1)::stacktail -> processor ((comlist1tail)::comliststail) ((STRING(x1)::stacktail)::sostail) env funstack 
                                                                                                | CLOSURE(x1, x2, x3, x4)::stacktail -> processor ((comlist1tail)::comliststail) (((str2sv (sv2str x1))::stacktail)::sostail) env funstack 
                                                                                            )
                                                                            )
                                                            | PRINTLN -> (match stackofstacks with
                                                                        | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::[])::stackofstacks) env funstack 
                                                                        | stack::sostail -> (match stack with
                                                                                                | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                | STRING(x1)::stacktail -> print_string (x1^"\n");file_write x1; processor ((comlist1tail)::comliststail) ((stacktail)::sostail) env funstack 
                                                                                                | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                            )
                                                                        )
                                                            | QUIT -> print_string "end of program" (*this print statement is for testing purposes*)
                                                            | CAT -> (match stackofstacks with
                                                                        | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::[])::stackofstacks) env funstack 
                                                                        | stack::sostail -> (match stack with
                                                                                                | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                | STRING(x1)::STRING(x2)::stacktail -> processor ((comlist1tail)::comliststail) ((STRING(x2 ^ x1)::stacktail)::sostail) env funstack 
                                                                                                | NAME(x1)::STRING(x2)::stacktail -> (match (fetch (NAME(x1)) env) with
                                                                                                                                        | STRING(s1) -> processor ((comlist1tail)::comliststail) ((STRING(x2 ^ s1)::stacktail)::sostail) env funstack 
                                                                                                                                        | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                        )
                                                                                                | STRING(x1)::NAME(x2)::stacktail -> (match (fetch (NAME(x2)) env) with
                                                                                                                                        | STRING(s2) -> processor ((comlist1tail)::comliststail) ((STRING(s2 ^ x1)::stacktail)::sostail) env funstack 
                                                                                                                                        | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                        )
                                                                                                | NAME(x1)::NAME(x2)::stacktail -> (match ((fetch (NAME(x1)) env), (fetch (NAME(x2)) env)) with
                                                                                                                                        | (STRING(s1), STRING(s2)) -> processor ((comlist1tail)::comliststail) ((STRING(s2 ^ s1)::stacktail)::sostail) env funstack 
                                                                                                                                        | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                        )
                                                                                                | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                            )
                                                                    )
                                                            | AND -> (match stackofstacks with
                                                                        | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::[])::stackofstacks) env funstack 
                                                                        | stack::sostail -> (match stack with
                                                                                                | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                | BOOL(true)::BOOL(true)::stacktail -> processor ((comlist1tail)::comliststail) ((BOOL(true)::stacktail)::sostail) env funstack 
                                                                                                | BOOL(x1)::BOOL(x2)::stacktail -> processor ((comlist1tail)::comliststail) ((BOOL(false)::stacktail)::sostail) env funstack 
                                                                                                | NAME(x1)::BOOL(true)::stacktail -> (match (fetchterminal (NAME(x1)) env) with
                                                                                                                                        | BOOL(true) -> processor ((comlist1tail)::comliststail) ((BOOL(true)::stacktail)::sostail) env funstack 
                                                                                                                                        | BOOL(false) -> processor ((comlist1tail)::comliststail) ((BOOL(false)::stacktail)::sostail) env funstack 
                                                                                                                                        | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                        )
                                                                                                | NAME(x1)::BOOL(false)::stacktail -> (match (fetchterminal (NAME(x1)) env) with
                                                                                                                                        | BOOL(b1) -> processor ((comlist1tail)::comliststail) ((BOOL(false)::stacktail)::sostail) env funstack 
                                                                                                                                        | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                        )
                                                                                                | BOOL(true)::NAME(x2)::stacktail -> (match (fetchterminal (NAME(x2)) env) with
                                                                                                                                        | BOOL(true) -> processor ((comlist1tail)::comliststail) ((BOOL(true)::stacktail)::sostail) env funstack 
                                                                                                                                        | BOOL(false) -> processor ((comlist1tail)::comliststail) ((BOOL(false)::stacktail)::sostail) env funstack 
                                                                                                                                        | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                        )
                                                                                                | BOOL(false)::NAME(x2)::stacktail -> (match (fetchterminal (NAME(x2)) env) with
                                                                                                                                        | BOOL(b2) -> processor ((comlist1tail)::comliststail) ((BOOL(false)::stacktail)::sostail) env funstack 
                                                                                                                                        | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                        )
                                                                                                | NAME(x1)::NAME(x2)::stacktail -> (match ((fetchterminal (NAME(x1)) env), (fetchterminal (NAME(x2)) env)) with
                                                                                                                                        | (BOOL(true), BOOL(true)) -> processor ((comlist1tail)::comliststail) ((BOOL(true)::stacktail)::sostail) env funstack 
                                                                                                                                        | (BOOL(b1), BOOL(b2)) -> processor ((comlist1tail)::comliststail) ((BOOL(false)::stacktail)::sostail) env funstack 
                                                                                                                                        | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                        )
                                                                                                | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                            )
                                                                    )
                                                            | OR -> (match stackofstacks with
                                                                        | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::[])::stackofstacks) env funstack 
                                                                        | stack::sostail -> (match stack with
                                                                                                | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                | BOOL(false)::BOOL(false)::stacktail -> processor ((comlist1tail)::comliststail) ((BOOL(false)::stacktail)::sostail) env funstack 
                                                                                                | BOOL(x1)::BOOL(x2)::stacktail -> processor ((comlist1tail)::comliststail) ((BOOL(true)::stacktail)::sostail) env funstack 
                                                                                                | NAME(x1)::BOOL(true)::stacktail -> (match (fetchterminal (NAME(x1)) env) with
                                                                                                                                        | BOOL(b1) -> processor ((comlist1tail)::comliststail) ((BOOL(true)::stacktail)::sostail) env funstack 
                                                                                                                                        | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                        )
                                                                                                | NAME(x1)::BOOL(false)::stacktail -> (match (fetchterminal (NAME(x1)) env) with
                                                                                                                                        | BOOL(true) -> processor ((comlist1tail)::comliststail) ((BOOL(true)::stacktail)::sostail) env funstack 
                                                                                                                                        | BOOL(false) -> processor ((comlist1tail)::comliststail) ((BOOL(false)::stacktail)::sostail) env funstack 
                                                                                                                                        | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                        )
                                                                                                | BOOL(true)::NAME(x2)::stacktail -> (match (fetchterminal (NAME(x2)) env) with
                                                                                                                                        | BOOL(b2) -> processor ((comlist1tail)::comliststail) ((BOOL(true)::stacktail)::sostail) env funstack 
                                                                                                                                        | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                        )
                                                                                                | BOOL(false)::NAME(x2)::stacktail -> (match (fetchterminal (NAME(x2)) env) with
                                                                                                                                        | BOOL(true) -> processor ((comlist1tail)::comliststail) ((BOOL(true)::stacktail)::sostail) env funstack 
                                                                                                                                        | BOOL(false) -> processor ((comlist1tail)::comliststail) ((BOOL(false)::stacktail)::sostail) env funstack 
                                                                                                                                        | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                        )
                                                                                                | NAME(x1)::NAME(x2)::stacktail -> (match ((fetchterminal (NAME(x1)) env), (fetchterminal (NAME(x2)) env)) with
                                                                                                                                        | (BOOL(false), BOOL(false)) -> processor ((comlist1tail)::comliststail) ((BOOL(false)::stacktail)::sostail) env funstack 
                                                                                                                                        | (BOOL(b1), BOOL(b2)) -> processor ((comlist1tail)::comliststail) ((BOOL(true)::stacktail)::sostail) env funstack 
                                                                                                                                        | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                        )
                                                                                                | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                            )
                                                                    )
                                                            | NOT -> (match stackofstacks with
                                                                        | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::[])::stackofstacks) env funstack 
                                                                        | stack::sostail -> (match stack with
                                                                                                | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                | BOOL(true)::stacktail -> processor ((comlist1tail)::comliststail) ((BOOL(false)::stacktail)::sostail) env funstack 
                                                                                                | BOOL(false)::stacktail -> processor ((comlist1tail)::comliststail) ((BOOL(true)::stacktail)::sostail) env funstack 
                                                                                                | NAME(x1)::stacktail -> (match (fetchterminal (NAME(x1)) env) with
                                                                                                                            | BOOL(true) -> processor ((comlist1tail)::comliststail) ((BOOL(false)::stacktail)::sostail) env funstack 
                                                                                                                            | BOOL(false) -> processor ((comlist1tail)::comliststail) ((BOOL(true)::stacktail)::sostail) env funstack 
                                                                                                                            | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                            )
                                                                                                | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                            )
                                                                    )
                                                            | EQUAL -> (match stackofstacks with
                                                                        | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::[])::stackofstacks) env funstack 
                                                                        | stack::sostail -> (match stack with
                                                                                                | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                | INT(x1)::INT(x2)::stacktail -> (if x2 = x1
                                                                                                                                    then processor ((comlist1tail)::comliststail) ((BOOL(true)::stacktail)::sostail) env funstack 
                                                                                                                                else
                                                                                                                                    processor ((comlist1tail)::comliststail) ((BOOL(false)::stacktail)::sostail) env funstack 
                                                                                                                                    )
                                                                                                | NAME(x1)::INT(x2)::stacktail -> (match (fetchterminal (NAME(x1)) env) with
                                                                                                                                    | INT(i1) -> (if x2 = i1
                                                                                                                                                        then processor ((comlist1tail)::comliststail) ((BOOL(true)::stacktail)::sostail) env funstack 
                                                                                                                                                    else
                                                                                                                                                        processor ((comlist1tail)::comliststail) ((BOOL(false)::stacktail)::sostail) env funstack 
                                                                                                                                                    )
                                                                                                                                    | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                    )
                                                                                                | INT(x1)::NAME(x2)::stacktail -> (match (fetchterminal (NAME(x2)) env) with
                                                                                                                                    | INT(i2) -> (if i2 = x1
                                                                                                                                                        then processor ((comlist1tail)::comliststail) ((BOOL(true)::stacktail)::sostail) env funstack 
                                                                                                                                                    else
                                                                                                                                                        processor ((comlist1tail)::comliststail) ((BOOL(false)::stacktail)::sostail) env funstack 
                                                                                                                                                    )
                                                                                                                                    | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                    )
                                                                                                | NAME(x1)::NAME(x2)::stacktail -> (match ((fetchterminal (NAME(x1)) env), (fetchterminal (NAME(x2)) env)) with
                                                                                                                                    | (INT(i1), INT(i2)) -> (if i2 = i1
                                                                                                                                                                    then processor ((comlist1tail)::comliststail) ((BOOL(true)::stacktail)::sostail) env funstack 
                                                                                                                                                                else
                                                                                                                                                                    processor ((comlist1tail)::comliststail) ((BOOL(false)::stacktail)::sostail) env funstack 
                                                                                                                                                    )
                                                                                                                                    | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                    )
                                                                                                | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                            )
                                                                        )
                                                            | LESSTHAN -> (match stackofstacks with
                                                                            | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::[])::stackofstacks) env funstack 
                                                                            | stack::sostail -> (match stack with
                                                                                                    | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                    | INT(x1)::INT(x2)::stacktail -> (if x2 < x1
                                                                                                                                        then processor ((comlist1tail)::comliststail) ((BOOL(true)::stacktail)::sostail) env funstack 
                                                                                                                                    else
                                                                                                                                        processor ((comlist1tail)::comliststail) ((BOOL(false)::stacktail)::sostail) env funstack 
                                                                                                                                        )
                                                                                                    | NAME(x1)::INT(x2)::stacktail -> (match (fetchterminal (NAME(x1)) env) with
                                                                                                                                        | INT(i1) -> (if x2 < i1
                                                                                                                                                            then processor ((comlist1tail)::comliststail) ((BOOL(true)::stacktail)::sostail) env funstack 
                                                                                                                                                        else
                                                                                                                                                            processor ((comlist1tail)::comliststail) ((BOOL(false)::stacktail)::sostail) env funstack 
                                                                                                                                                        )
                                                                                                                                        | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                        )
                                                                                                    | INT(x1)::NAME(x2)::stacktail -> (match (fetchterminal (NAME(x2)) env) with
                                                                                                                                        | INT(i2) -> (if i2 < x1
                                                                                                                                                            then processor ((comlist1tail)::comliststail) ((BOOL(true)::stacktail)::sostail) env funstack 
                                                                                                                                                        else
                                                                                                                                                            processor ((comlist1tail)::comliststail) ((BOOL(false)::stacktail)::sostail) env funstack 
                                                                                                                                                        )
                                                                                                                                        | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                        )
                                                                                                    | NAME(x1)::NAME(x2)::stacktail -> (match ((fetchterminal (NAME(x1)) env), (fetchterminal (NAME(x2)) env)) with
                                                                                                                                        | (INT(i1), INT(i2)) -> (if i2 < i1
                                                                                                                                                                        then processor ((comlist1tail)::comliststail) ((BOOL(true)::stacktail)::sostail) env funstack 
                                                                                                                                                                    else
                                                                                                                                                                        processor ((comlist1tail)::comliststail) ((BOOL(false)::stacktail)::sostail) env funstack 
                                                                                                                                                        )
                                                                                                                                        | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                        )
                                                                                                    | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                )
                                                                            )
                                                            | BIND -> (match stackofstacks with
                                                                        | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::[])::stackofstacks) env funstack 
                                                                        | stack::sostail -> (match stack with
                                                                                                | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                | INT(x1)::NAME(x2)::stacktail -> processor ((comlist1tail)::comliststail) ((UNIT(":unit:")::stacktail)::sostail) (bind_value (INT(x1)) (NAME(x2)) env) funstack
                                                                                                | STRING(x1)::NAME(x2)::stacktail -> processor ((comlist1tail)::comliststail) ((UNIT(":unit:")::stacktail)::sostail) (bind_value (STRING(x1)) (NAME(x2)) env) funstack
                                                                                                | BOOL(x1)::NAME(x2)::stacktail -> processor ((comlist1tail)::comliststail) ((UNIT(":unit:")::stacktail)::sostail) (bind_value (BOOL(x1)) (NAME(x2)) env) funstack
                                                                                                | UNIT(x1)::NAME(x2)::stacktail -> processor ((comlist1tail)::comliststail) ((UNIT(":unit:")::stacktail)::sostail) (bind_value (UNIT(x1)) (NAME(x2)) env) funstack
                                                                                                | NAME(x1)::NAME(x2)::stacktail -> (match (fetch (NAME(x1)) env) with                                        
                                                                                                                                    | ERROR(":error:") -> processor ((comlist1tail)::comliststail) ((UNIT(":unit:")::stacktail)::sostail) (bind_value (NAME(x1)) (NAME(x2)) env) funstack
                                                                                                                                    | _ -> processor ((comlist1tail)::comliststail) ((UNIT(":unit:")::stacktail)::sostail) (bind_value (fetchterminal (NAME(x1)) env) (NAME(x2)) env) funstack
                                                                                                                                    )
                                                                                                | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                            )
                                                                        )
                                                            | IF -> (match stackofstacks with
                                                                    | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::[])::stackofstacks) env funstack 
                                                                    | stack::sostail -> (match stack with
                                                                                            | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                            | s1::s2::BOOL(true)::stacktail -> processor ((comlist1tail)::comliststail) ((s1::stacktail)::sostail) env funstack 
                                                                                            | s1::s2::BOOL(false)::stacktail -> processor ((comlist1tail)::comliststail) ((s2::stacktail)::sostail) env funstack 
                                                                                            | s1::s2::NAME(x3)::stacktail -> (match (fetchterminal (NAME(x3)) env) with
                                                                                                                                | BOOL(true) -> processor ((comlist1tail)::comliststail) ((s1::stacktail)::sostail) env funstack 
                                                                                                                                | BOOL(false) -> processor ((comlist1tail)::comliststail) ((s2::stacktail)::sostail) env funstack 
                                                                                                                                | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                )
                                                                                            | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                        )
                                                                        )
                                                            | LET -> (match stackofstacks with
                                                                        | [] -> processor ((comlist1tail)::comliststail) (([])::stackofstacks) (([])::env) funstack
                                                                        | stack::sostail -> processor ((comlist1tail)::comliststail) (([])::stackofstacks) (([])::env) funstack
                                                                        )
                                                            | END -> (match stackofstacks with
                                                                        | [] -> (match env with
                                                                                    | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::[])::stackofstacks) env funstack 
                                                                                    | e1::envtail -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::[])::stackofstacks) envtail funstack
                                                                                )
                                                                        | stack::prevstack::sostail -> (match (stack, env) with
                                                                                                        | ([], []) -> processor ((comlist1tail)::comliststail) ((prevstack)::sostail) env funstack 
                                                                                                        | ([], (e1::envtail)) -> processor ((comlist1tail)::comliststail) ((prevstack)::sostail) envtail funstack
                                                                                                        | ((s1::stacktail), []) -> processor ((comlist1tail)::comliststail) ((s1::prevstack)::sostail) env funstack
                                                                                                        | ((s1::stacktail), (e1::envtail)) -> processor ((comlist1tail)::comliststail) ((s1::prevstack)::sostail) envtail funstack
                                                                                                        )
                                                                        | stack::sostail -> (match (stack, env) with
                                                                                                | ([], []) -> processor ((comlist1tail)::comliststail) (sostail) env funstack
                                                                                                | ([], (e1::envtail)) -> processor ((comlist1tail)::comliststail) sostail envtail funstack
                                                                                                | ((s1::stacktail), []) -> processor ((comlist1tail)::comliststail) ((s1::[])::sostail) env funstack
                                                                                                | ((s1::stacktail), (e1::envtail)) -> processor ((comlist1tail)::comliststail) ((s1::[])::sostail) envtail funstack
                                                                                                )
                                                                        | _ -> (match env with
                                                                                    | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::[])::stackofstacks) env funstack
                                                                                    | e1::envtail -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::[])::stackofstacks) envtail funstack
                                                                                )
                                                                        )
                                                            (*takes away commands for body of function, creates a (fname, CLOSURE) definition in current scope/environment*)
                                                            | FUN (fname, param) -> (match env with
                                                                                        | [] -> processor ((funbodytail comlist1tail 0)::comliststail) (stackofstacks) (((fname, CLOSURE(param, (map com2str (funbody comlist1tail 0)), (listlist_to_List env []), FUNF))::[])::env) funstack
                                                                                        | e1::envtail -> processor ((funbodytail comlist1tail 0)::comliststail) (stackofstacks) (((fname, CLOSURE(param, (map com2str (funbody comlist1tail 0)), (listlist_to_List env []), FUNF))::e1)::envtail) funstack

                                                                                        )
                                                            (*pops FUNEND from comlist1 and pushes UNIT to the stack*)
                                                            | FUNEND -> (match stackofstacks with
                                                                        | [] -> processor ((comlist1tail)::comliststail) ((UNIT(":unit:")::[])::stackofstacks) env funstack 
                                                                        | stack::sostail -> processor ((comlist1tail)::comliststail) ((UNIT(":unit:")::stack)::sostail) env funstack 
                                                                        )
                                                            | RETURN -> (match stackofstacks with
                                                                        | [] -> (match env with
                                                                                    | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::[])::stackofstacks) env funstack
                                                                                    | e1::envtail -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::[])::stackofstacks) env funstack
                                                                                )
                                                                        | stack::prevstack::sostail -> (match stack with
                                                                                                            | [] -> processor ((comlist1tail)::comliststail) (stack::(ERROR(":error:")::prevstack)::sostail) env funstack
                                                                                                            | s1::stacktail -> (match s1 with
                                                                                                                                        | NAME(x1) -> (match (fetchterminal s1 env) with
                                                                                                                                                        | ERROR(b1) -> processor ((comlist1tail)::comliststail) (stack::(ERROR("::error:")::prevstack)::sostail) env funstack
                                                                                                                                                        | CLOSURE(b1, b2, b3, b4) -> (match env with
                                                                                                                                                                                        | [] -> processor ((comlist1tail)::comliststail) (stack::(s1::prevstack)::sostail) (bind_value (CLOSURE(b1, b2, b3, b4)) s1 env) funstack
                                                                                                                                                                                        | e1::envtail -> processor ((comlist1tail)::comliststail) (stack::(s1::prevstack)::sostail) (e1::(bind_value (CLOSURE(b1, b2, b3, b4)) s1 envtail)) funstack
                                                                                                                                                                                        )
                                                                                                                                                        | _ -> processor ((comlist1tail)::comliststail) (stack::((fetchterminal s1 env)::prevstack)::sostail) env funstack
                                                                                                                                                        )
                                                                                                                                        | _ -> processor ((comlist1tail)::comliststail) (stack::(s1::prevstack)::sostail) env funstack
                                                                                                                                        )
                                                                                                            )
                                                                        | stack::sostail -> (match stack with
                                                                                                | [] -> processor ((comlist1tail)::comliststail) (stack::(ERROR(":error:")::[])::sostail) env funstack
                                                                                                | s1::stacktail -> (match s1 with
                                                                                                                            | NAME(x1) -> (match (fetchterminal s1 env) with
                                                                                                                                            | ERROR(b1) -> processor ((comlist1tail)::comliststail) (stack::(ERROR("::error:")::[])::sostail) env funstack
                                                                                                                                            | CLOSURE(b1, b2, b3, b4) -> (match env with
                                                                                                                                                                            | [] -> processor ((comlist1tail)::comliststail) (stack::(s1::[])::sostail) (bind_value (CLOSURE(b1, b2, b3, b4)) s1 env) funstack
                                                                                                                                                                            | e1::envtail -> processor ((comlist1tail)::comliststail) (stack::(s1::[])::sostail) (e1::(bind_value (CLOSURE(b1, b2, b3, b4)) s1 envtail)) funstack
                                                                                                                                                                            )
                                                                                                                                            | _ -> processor ((comlist1tail)::comliststail) (stack::((fetchterminal s1 env)::[])::sostail) env funstack
                                                                                                                                            )
                                                                                                                            | _ -> processor ((comlist1tail)::comliststail) (stack::(s1::[])::sostail) env funstack
                                                                                                                            )
                                                                                                )
                                                                        )
                                                            | CALL -> (match stackofstacks with
                                                                        | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::[])::stackofstacks) env funstack 
                                                                        | stack::sostail -> (match stack with
                                                                                                | [] -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                | NAME(arg)::NAME(fname)::stacktail -> (match ((fetch (NAME(arg)) env), (fetch (NAME(fname)) env)) with
                                                                                                                                        | (ERROR(x), CLOSURE(x1, x2, x3, x4)) -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                        | (argx1, NAME(x2)) -> (match (fetch (fetchfun (NAME(x2)) env) env) with
                                                                                                                                                                    | CLOSURE(f1, f2, f3, f4) -> (match f4 with
                                                                                                                                                                                                    | FUNF -> processor ((List.append (map str2com f2) ((CALLEND)::comlist1tail))::comliststail) (([])::stacktail::sostail) (bind_value (fetchfun (NAME(arg)) env) f1 (f3::env)) ((fetchfun (NAME(x2)) env)::funstack)
                                                                                                                                                                                                    | FUNIO -> processor ((List.append (map str2com f2) ((CALLEND)::comlist1tail))::comliststail) (([])::stacktail::sostail) (f3::(bind_value (fetchfun (NAME(arg)) env) f1 env)) ((fetchfun (NAME(x2)) env)::funstack)
                                                                                                                                                                                                    )
                                                                                                                                                                    | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                                                                    
                                                                                                                                                                    )
                                                                                                                                        | (argx1, CLOSURE(x1, x2, x3, x4)) -> (match x4 with
                                                                                                                                                                                | FUNF -> processor ((List.append (map str2com x2) ((CALLEND)::comlist1tail))::comliststail) (([])::stacktail::sostail) (bind_value (NAME(arg)) x1 (x3::env)) (NAME(fname)::funstack)
                                                                                                                                                                                | FUNIO -> processor ((List.append (map str2com x2) ((CALLEND)::comlist1tail))::comliststail) (([])::stacktail::sostail) (x3::(bind_value (NAME(arg)) x1 env)) (NAME(fname)::funstack)
                                                                                                                                                                                )
                                                                                                                                        | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                        )
                                                                                                | INT(arg)::NAME(fname)::stacktail -> (match (fetch (NAME(fname)) env) with
                                                                                                                                        | CLOSURE(x1, x2, x3, x4) -> (match x4 with
                                                                                                                                                                        | FUNF -> processor ((List.append (map str2com x2) ((CALLEND)::comlist1tail))::comliststail) (([])::stacktail::sostail) (bind_value (INT(arg)) x1 (x3::env)) (NAME(fname)::funstack)
                                                                                                                                                                        | FUNIO -> processor ((List.append (map str2com x2) ((CALLEND)::comlist1tail))::comliststail) (([])::stacktail::sostail) (x3::(bind_value (INT(arg)) x1 env)) (NAME(fname)::funstack)
                                                                                                                                                                        )
                                                                                                                                        | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                        )
                                                                                                | STRING(arg)::NAME(fname)::stacktail -> (match (fetch (NAME(fname)) env) with
                                                                                                                                        | CLOSURE(x1, x2, x3, x4) -> (match x4 with
                                                                                                                                                                        | FUNF -> processor ((List.append (map str2com x2) ((CALLEND)::comlist1tail))::comliststail) (([])::stacktail::sostail) (bind_value (STRING(arg)) x1 (x3::env)) (NAME(fname)::funstack)
                                                                                                                                                                        | FUNIO -> processor ((List.append (map str2com x2) ((CALLEND)::comlist1tail))::comliststail) (([])::stacktail::sostail) (x3::(bind_value (STRING(arg)) x1 env)) (NAME(fname)::funstack)
                                                                                                                                                                        )
                                                                                                                                        | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                        )
                                                                                                | BOOL(arg)::NAME(fname)::stacktail -> (match (fetch (NAME(fname)) env) with
                                                                                                                                        | CLOSURE(x1, x2, x3, x4) -> (match x4 with
                                                                                                                                                                        | FUNF -> processor ((List.append (map str2com x2) ((CALLEND)::comlist1tail))::comliststail) (([])::stacktail::sostail) (bind_value (BOOL(arg)) x1 (x3::env)) (NAME(fname)::funstack)
                                                                                                                                                                        | FUNIO -> processor ((List.append (map str2com x2) ((CALLEND)::comlist1tail))::comliststail) (([])::stacktail::sostail) (x3::(bind_value (BOOL(arg)) x1 env)) (NAME(fname)::funstack)
                                                                                                                                                                        )
                                                                                                                                        | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack
                                                                                                                                        )
                                                                                                | UNIT(arg)::NAME(fname)::stacktail -> (match (fetch (NAME(fname)) env) with
                                                                                                                                        | CLOSURE(x1, x2, x3, x4) -> (match x4 with
                                                                                                                                                                        | FUNF -> processor ((List.append (map str2com x2) ((CALLEND)::comlist1tail))::comliststail) (([])::stacktail::sostail) (bind_value (UNIT(arg)) x1 (x3::env)) (NAME(fname)::funstack)
                                                                                                                                                                        | FUNIO -> processor ((List.append (map str2com x2) ((CALLEND)::comlist1tail))::comliststail) (([])::stacktail::sostail) (x3::(bind_value (UNIT(arg)) x1 env)) (NAME(fname)::funstack)
                                                                                                                                                                        )
                                                                                                                                        | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack
                                                                                                                                        )
                                                                                                | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack
                                                                                                )
                                                                        )
                                                            | CALLEND -> (match stackofstacks with
                                                                            | [] -> processor ((comlist1tail)::comliststail) stackofstacks env funstack 
                                                                            | stack::sostail -> (match env with
                                                                                                    | [] -> processor ((comlist1tail)::comliststail) stackofstacks env funstack 
                                                                                                    | e1::envtail -> (match funstack with
                                                                                                                        | [] -> processor ((comlist1tail)::comliststail) stackofstacks env funstack 
                                                                                                                        | f1::funstacktail -> (match (fetch f1 env) with
                                                                                                                                                | CLOSURE(x1, x2, x3, x4) -> (match x4 with
                                                                                                                                                                                | FUNF -> processor ((comlist1tail)::comliststail) (sostail) (envtail) funstacktail
                                                                                                                                                                                | FUNIO -> processor ((comlist1tail)::comliststail) (sostail) (bind_value (fetchscope x1 e1) (fetch x1 envtail) envtail) funstacktail
                                                                                                                                                                                )
                                                                                                                                                | _ -> processor ((comlist1tail)::comliststail) ((ERROR(":error:")::stack)::sostail) env funstack 
                                                                                                                                                )
                                                                                                                        )
                                                                                                    )
                                                                            )
                                                            | INOUTFUN(fname, param) -> (match env with
                                                                                        | [] -> processor ((funbodytail comlist1tail 0)::comliststail) (stackofstacks) (((fname, CLOSURE(param, (map com2str (funbody comlist1tail 0)), (listlist_to_List env []), FUNIO))::[])::env) funstack
                                                                                        | e1::envtail -> processor ((funbodytail comlist1tail 0)::comliststail) (stackofstacks) (((fname, CLOSURE(param, (map com2str (funbody comlist1tail 0)), (listlist_to_List env []), FUNIO))::e1)::envtail) funstack
                                                                                        )
                                                            )
                                    )
    in

    processor comListList [] [] [];

;;

(*for testing in the terminal*)
(*make sure to re-comment this before submitting on autolab*)
interpreter ("inputtest.txt","testoutput1.txt")
