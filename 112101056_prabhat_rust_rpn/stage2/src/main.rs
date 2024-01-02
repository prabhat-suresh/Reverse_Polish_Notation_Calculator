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
    let mut stack: Vec<f64> = vec![];
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
                if strng == "/" && top2 == 0.0 {
                    println!("NAN");
                    return;
                }
                stack.push(eval(top1, top2, strng));
            }
            _ => {
                let num: f64 = match strng.trim().parse() {
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
        println!("{:.5}", stack[0]);
    }
}

fn eval(x: f64, y: f64, op: &str) -> f64 {
    match op {
        "+" => x + y,
        "-" => x - y,
        "*" => x * y,
        "^" => f64::powf(x, y),
        "/" => x / y,
        _ => 0.0,
    }
}
