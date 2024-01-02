use std::io;
use std::process;

struct Polynomial {
    //a list of coefficient-exponent pairs
    poly: Vec<(f64, i64)>,
}

impl Polynomial {
    fn add(&mut self, other: &Polynomial) {
        //merging two polynomials in ascending order of their powers
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
        //same as adding the negative of the second polynomial
        let mut p = other.poly.clone();
        for i in 0..p.len() {
            p[i].0 = -p[i].0;
        }
        self.add(&Polynomial { poly: p });
    }

    fn mul(&mut self, other: &Polynomial) {
        //adding the products obtained from multiplying each element of one polynomial with the other polynomial
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
    // with 3 squaring operation this is better than the naive O(n^2) algo.
    // T(n)=3*T(n/2) + O(n) -----> Complexity: O(n^lg3)

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
        //applying binary exponentiation for efficient computation
        if e == 0 {
            self.poly = vec![(1.0, 0)];
            return;
        }
        if e == 1 {
            return;
        }
        //using sq for large polynomials and mul otherwise
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
        //changing each term a*(x^n) to a*n*(x^(n-1))
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
            print!("0 ");
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
    }
}

struct RationalPolynomial {
    //rational polynomials are of the form p(x)/q(x) where q(x)!=0
    p: Polynomial,
    q: Polynomial,
}

impl RationalPolynomial {
    //implementing all operations for rational polynomials using methods for polynomials
    fn add_helper(&mut self, other: &RationalPolynomial, subtract: bool) {
        let p1 = &mut self.p;
        let p2 = &other.p;
        let q1 = &mut self.q;
        let q2 = &other.q;

        p1.mul(q2);
        let mut p2copy = Polynomial {
            poly: p2.poly.clone(),
        };
        if subtract {
            for t in p2copy.poly.iter_mut() {
                *t = (-t.0, t.1);
            }
        }
        p2copy.mul(q1);
        p1.add(&p2copy);
        q1.mul(q2);
    }

    fn add(&mut self, other: &RationalPolynomial) {
        self.add_helper(other, false);
    }

    fn sub(&mut self, other: &RationalPolynomial) {
        self.add_helper(other, true);
    }

    fn mul(&mut self, other: &RationalPolynomial) {
        let p1 = &mut self.p;
        let p2 = &other.p;
        let q1 = &mut self.q;
        let q2 = &other.q;

        p1.mul(p2);
        q1.mul(q2);
    }

    fn div(&mut self, other: &RationalPolynomial) {
        let p1 = &mut self.p;
        let p2 = &other.p;
        let q1 = &mut self.q;
        let q2 = &other.q;

        p1.mul(q2);
        q1.mul(p2);
    }

    fn exp(&mut self, mut e: i64) {
        if e < 0 {
            //take reciprocal
            e = -e;
            let pcopy = self.p.poly.clone();
            let qcopy = self.q.poly.clone();
            self.p = Polynomial { poly: qcopy };
            self.q = Polynomial { poly: pcopy };
        }
        let p = &mut self.p;
        let q = &mut self.q;
        p.exp(e as u64);
        q.exp(e as u64);
    }

    fn diff(&mut self) {
        let p = Polynomial {
            poly: self.p.poly.clone(),
        };
        let mut q = Polynomial {
            poly: self.q.poly.clone(),
        };
        self.p.diff();
        self.p.mul(&q);
        q.diff();
        q.mul(&p);
        self.p.sub(&q);
        self.q.exp(2);
    }

    fn simplify(&mut self, flag: bool) -> Polynomial {
        // reduce p(x)/q(x) ----> p1(x)+p'(x)/q'(x) where degree(p'(x))<degree(q'(x)) if flag = false
        // and degree(p'(x))<=degree(q'(x)) if flag = true
        let mut res = vec![];
        let p = &mut self.p;
        let q = &self.q;
        let (coeffq, degreeq) = (q.poly.last().unwrap().0, q.poly.last().unwrap().1);
        loop {
            let (coeffp, degreep) = match p.poly.last() {
                Some(t) => (t.0, t.1),
                None => break,
            };
            if coeffp == 0.0 {
                p.poly.pop();
                continue;
            }
            if degreeq > degreep {
                break;
            }
            if flag && degreep == degreeq {
                break;
            }
            res.push((coeffp / coeffq, degreep - degreeq));
            let mut q1 = Polynomial {
                poly: q.poly.clone(),
            };
            q1.mul(&Polynomial {
                poly: vec![(coeffp / coeffq, degreep - degreeq)],
            });
            p.sub(&q1);
        }
        if self.q.poly.len() == 1 {
            self.p.mul(&Polynomial {
                poly: vec![(1.0 / self.q.poly[0].0, -self.q.poly[0].1)],
            });
            self.q.poly = vec![(1.0, 0)];
        }
        Polynomial { poly: res }
    }

    fn print_rational_poly(&mut self) {
        let mut flag = true;
        let mut v = self.simplify(true);
        v.poly.reverse();
        if self.p.poly.len() == 0 || self.p.poly.len() == 1 && self.p.poly[0].0 == 0.0 {
            flag = false;
        } else {
            self.p.print_poly();
            if self.q.poly.len() != 1 {
                self.q.print_poly();
                print!("/ ");
            }
        }
        if !flag || !(v.poly.len() == 0 || v.poly.len() == 1 && v.poly[0].0 == 0.0) {
            v.print_poly();
            if flag {
                print!("+");
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
    let mut stack: Vec<RationalPolynomial> = Vec::new();
    for strng in inp.split(' ') {
        match strng {
            "+" | "-" | "*" | "^" | "/" => {
                //binary operators
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

                if strng == "/"
                    && (top2.p.poly.len() == 0 || top2.p.poly.len() == 1 && top2.p.poly[0].0 == 0.0)
                {
                    println!("NAN");
                    return;
                }

                if strng == "^" {
                    let v = top2.simplify(false);
                    if !(v.poly.len() == 1 && v.poly[0].1 == 0 && top2.p.poly.len() == 0) {
                        println!("Parse error");
                        return;
                    }
                    top2.p = v;
                }
                eval(&mut top1, &top2, strng);
                stack.push(top1);
            }
            "d" => {
                //unary operator
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
                stack.push(RationalPolynomial {
                    p: Polynomial {
                        poly: vec![(1.0, 1)],
                    },
                    q: Polynomial {
                        poly: vec![(1.0, 0)],
                    },
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
                stack.push(RationalPolynomial {
                    p: Polynomial {
                        poly: vec![(num, 0)],
                    },
                    q: Polynomial {
                        poly: vec![(1.0, 0)],
                    },
                });
            }
        }
    }
    if stack.len() != 1 {
        println!("Parse error");
    } else {
        stack[0].print_rational_poly();
    }
}

fn eval(x: &mut RationalPolynomial, y: &RationalPolynomial, op: &str) {
    match op {
        "+" => x.add(y),
        "-" => x.sub(y),
        "*" => x.mul(y),
        "/" => x.div(y),
        "^" => {
            let pow = y.p.poly[0].0;
            x.exp(pow as i64);
        }
        _ => {}
    }
}
