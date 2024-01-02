use std::io;
use std::process;

fn main() {
    loop {
        let mut inp = String::new();
        match io::stdin().read_line(&mut inp) {
            Ok(0) | Err(_) => break,
            Ok(_) => {
                inp.pop();
                rpn(inp);
            }
        }
    }
    process::exit(0);
}

fn rpn(inp: String) {
    let mut stack: Vec<i128> = vec![];
    for strng in inp.split(' ') {
        match strng {
            "+" | "-" | "*" | "/" | "^" => {
                let top2 = match stack.pop() {
                    Some(i) => i,
                    None => {
                        println!("Parse error");
                        return;
                    }
                };
                let top1 = match stack.pop() {
                    Some(i) => i,
                    None => {
                        println!("Parse error");
                        return;
                    }
                };
                if strng == "/" && top2 == 0 {
                    println!("NAN");
                    return;
                }
                stack.push(eval(top1, top2, strng));
            }
            _ => {
                let num: i128 = match strng.trim().parse() {
                    Ok(n) => n,
                    Err(_) => {
                        println!("Parse error");
                        return;
                    }
                };
                stack.push(num);
            }
        }
    }
    if stack.len() != 1 {
        println!("Parse error");
    } else {
        println!("{}", stack[0]);
    }
}

fn eval(x: i128, y: i128, op: &str) -> i128 {
    match op {
        "+" => x + y,
        "-" => x - y,
        "*" => x * y,
        "^" => i128::pow(
            x,
            match y.try_into() {
                Ok(val) => val,
                Err(_) => {
                    process::exit(1);
                }
            },
        ),
        "/" => x / y,
        _ => 0,
    }
}
