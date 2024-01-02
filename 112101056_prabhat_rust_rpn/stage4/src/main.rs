use std::io;
use std::process;

struct Polynomial {
    poly: Vec<(f64, i64)>,
}

impl Polynomial {
    fn add(&mut self, other: &Polynomial) {
        let mut sum = vec![];
        let poly1 = &(self.poly);
        let poly2 = &(other.poly);

        let mut i = 0;
        let mut j = 0;
        while i < poly1.len() || j < poly2.len() {
            if j == poly2.len() || i < poly1.len() && poly1[i].1 < poly2[j].1 {
                if poly1[i].0 != 0.0 {
                    sum.push(poly1[i]);
                }
                i = i + 1;
            } else if i == poly1.len() || poly2[j].1 < poly1[i].1 {
                if poly2[j].0 != 0.0 {
                    sum.push(poly2[j]);
                }
                j = j + 1;
            } else {
                if poly1[i].0 + poly2[j].0 != 0.0 {
                    sum.push((poly1[i].0 + poly2[j].0, poly1[i].1));
                }
                i = i + 1;
                j = j + 1;
            }
        }

        self.poly = sum;
    }

    fn sub(&mut self, other: &Polynomial) {
        let mut p = other.poly.clone();
        for i in 0..p.len() {
            p[i].0 = -p[i].0;
        }
        self.add(&Polynomial { poly: p });
    }

    fn mul(&mut self, other: &Polynomial) {
        let mut product = Polynomial { poly: vec![] };

        let p1 = &(self.poly);
        let p2 = &(other.poly);

        for t1 in p1 {
            let mut p = p2.clone();
            for t2 in p.iter_mut() {
                *t2 = (t1.0 * t2.0, t1.1 + t2.1);
            }
            product.add(&Polynomial { poly: p });
        }

        self.poly = product.poly;
    }
    // squaring a polynomial using Strassen's matrix multiplication like trick
    // p=p0p1 where p0 has terms with power < n/2 and p1 = (p-p1)/(x^(n/2)) where n is the degree of p
    // p^2 = (p0 + x^(n/2) * p1)^2 = p0^2 + x^n * p1^2 + x^(n/2) * (2*p0*p1)
    // = p0^2 + x^n * p1^2 + x^(n/2) * ((p0+p1)^2 - p0^2 - p1^2)
    // X=p0^2, Y=p1^2. Z=(p0+p1)^2
    // p^2 = X + x^n * Y + x^(n/2) * (Z - X - Y)
    // with 3 squaring operation this is better than the naive O(n^2) algo. Complexity: O(n^lg3)

    fn sq(&mut self) {
        let p = &mut self.poly;
        if p.len() == 0 {
            return;
        }
        if p.len() == 1 {
            p[0] = (p[0].0 * p[0].0, p[0].1 * 2);
            return;
        }
        let mut p0: Vec<(f64, i64)> = vec![];
        let mut p1: Vec<(f64, i64)> = vec![];
        let n = match p.last() {
            Some(t) => {
                let mut i = 2;
                while i < t.1 {
                    i = i * 2;
                }
                i
            }
            None => 0,
        };
        for t in p {
            if t.1 < n / 2 {
                p0.push(*t);
            } else {
                p1.push((t.0, t.1 - n / 2));
            }
        }
        let p0plusp1 = p0.clone();
        let mut z = Polynomial { poly: p0plusp1 };
        let mut x = Polynomial { poly: p0 };
        let mut y = Polynomial { poly: p1 };
        z.add(&y);
        z.sq();
        y.sq();
        x.sq();
        z.sub(&x);
        z.sub(&y);
        for t in y.poly.iter_mut() {
            *t = (t.0, t.1 + n);
        }
        for t in z.poly.iter_mut() {
            *t = (t.0, t.1 + n / 2);
        }
        x.add(&y);
        x.add(&z);
        self.poly = x.poly;
    }

    fn exp(&mut self, e: u64) {
        if e == 0 {
            self.poly = vec![(1.0, 0)];
            return;
        }
        if e == 1 {
            return;
        }
        if e % 2 == 1 {
            let p = self.poly.clone();
            self.exp(e / 2);
            if self.poly.len() <= 10000 {
                let half = self.poly.clone();
                self.mul(&Polynomial { poly: half });
            } else {
                self.sq();
            }
            self.mul(&Polynomial { poly: p });
        } else {
            self.exp(e / 2);
            if self.poly.len() <= 10000 {
                let half = self.poly.clone();
                self.mul(&Polynomial { poly: half });
            } else {
                self.sq();
            }
        }
    }

    fn diff(&mut self) {
        let mut i = 0;
        let mut flag = true;
        for t in self.poly.iter_mut() {
            *t = (t.0 * t.1 as f64, t.1 - 1);
            if t.0 == 0.0 {
                flag = false;
            }
            if flag {
                i = i + 1;
            }
        }
        if !flag {
            self.poly.remove(i);
        }
    }

    fn print_poly(&self) {
        let p = &self.poly;
        if p.len() == 0 {
            print!("0");
        } else if p[0].0 != 0.0 {
            if p[0].1 == 0 {
                print!("{:.5} ", p[0].0);
            } else if p[0].1 == 1 {
                if p[0].0 == 1.0 {
                    print!("x ");
                } else {
                    print!("x {:.5} * ", p[0].0);
                }
            } else if p[0].0 == 1.0 {
                print!("x {} ^ ", p[0].1);
            } else {
                print!("x {} ^ {:.5} * ", p[0].1, p[0].0);
            }
        } else if p.len() == 1 {
            print!("0 ");
        }
        for i in 1..p.len() {
            if p[i].0 != 0.0 {
                if p[i].1 == 0 {
                    print!("{:.5} + ", p[i].0);
                } else if p[i].1 == 1 {
                    if p[i].0 == 1.0 {
                        print!("x + ");
                    } else {
                        print!("x {:.5} * + ", p[i].0);
                    }
                } else if p[i].0 == 1.0 {
                    print!("x {} ^ + ", p[i].1);
                } else {
                    print!("x {} ^ {:.5} * + ", p[i].1, p[i].0);
                }
            }
        }
        println!("");
    }
}

fn main() {
    loop {
        let mut inp = String::new();
        match io::stdin().read_line(&mut inp) {
            Ok(0) | Err(_) => break,
            Ok(_) => {
                rpn(inp.trim());
            }
        }
    }
    process::exit(0);
}

fn rpn(inp: &str) {
    let mut stack: Vec<Polynomial> = Vec::new();
    for strng in inp.split(' ') {
        match strng {
            "+" | "-" | "*" | "^" => {
                let mut top2 = match stack.pop() {
                    Some(i) => i,
                    None => {
                        println!("Parse error");
                        return;
                    }
                };
                let mut top1 = match stack.pop() {
                    Some(i) => i,
                    None => {
                        println!("Parse error");
                        return;
                    }
                };

                if strng == "^" {
                    if top2.poly.len() == 0 {
                        top2.poly = vec![(0.0, 0)];
                    } else if top2.poly.len() != 1 || top2.poly[0].1 != 0 {
                        println!("Parse error");
                        return;
                    }
                }
                eval(&mut top1, &top2, strng);
                stack.push(top1);
            }
            "d" => {
                let mut top = match stack.pop() {
                    Some(i) => i,
                    None => {
                        println!("Parse error");
                        return;
                    }
                };
                top.diff();
                stack.push(top);
            }
            "x" => {
                stack.push(Polynomial {
                    poly: vec![(1.0, 1)],
                });
            }
            _ => {
                let strn = strng.trim();
                let num: f64 = match strn.parse() {
                    Ok(n) => {
                        let mut i = 0;
                        for c in strn.chars() {
                            if c == '.' {
                                if i == 0 || i + 6 != strn.len() {
                                    println!("Parse error");
                                    return;
                                } else {
                                    break;
                                }
                            }
                            i = i + 1;
                        }
                        n
                    }
                    Err(_) => {
                        println!("Parse error");
                        return;
                    }
                };
                stack.push(Polynomial {
                    poly: vec![(num, 0)],
                });
            }
        }
    }
    if stack.len() != 1 {
        println!("Parse error");
    } else {
        stack[0].print_poly();
    }
}

fn eval(x: &mut Polynomial, y: &Polynomial, op: &str) {
    match op {
        "+" => x.add(y),
        "-" => x.sub(y),
        "*" => x.mul(y),
        "^" => {
            let pow = y.poly[0].0;
            x.exp(pow as u64);
        }
        _ => {}
    }
}
