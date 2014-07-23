//! Special functions like table and plot points.

extern crate types;

use self::types::literal::{List, Matrix, Symbol, Void, LiteralType};
use super::super::{Expression, Evaluate, BadArgType, BadNumberOfArgs};
use super::{Environment, CalcResult, ArgType, Atom};

type Lit = LiteralType;
type Env = Environment;
type Expr = Expression;
pub type Table = Vec<(Vec<String>, String)>;
type Lists = Vec<Vec<Lit>>;

fn make_table(lists: Lists, names: Vec<String>, func: Expr, fun_str: String,
              env: &mut Env) -> CalcResult<(Table, Vec<uint>, uint)> {
    if lists.len() < 1 {
        fail!("make-table requires at least one list of variables")
    }

    let mut names_len = Vec::from_elem(names.len(), 0u);
    let (mut table, mut fn_len) = (Vec::new(), fun_str.len());
    table.push((names.clone(), fun_str));

    for column in range(0, lists[0].len()) {
        let mut child_env = Environment::new_frame(env);

        let values: Vec<Lit> = lists.iter().map(|x| x[column].clone()).collect();
        let mut t_names: Vec<String> = Vec::with_capacity(values.len());

        for val in range(0, values.len()) {
            let name = values[val].to_string();
            if name.len() > names_len[val] {
                *names_len.get_mut(val) = name.len();
            }
            t_names.push(name);
        }

        for (arg, val) in names.iter().zip(values.iter()) {
            child_env.symbols.insert(arg.clone(), val.clone());
        }

        let result = try!(try!(func.eval(&mut child_env)).desymbolize(env)).to_string();
        if result.len() > fn_len {
            fn_len = result.len();
        }

        table.push((t_names, result));
    }

    Ok((table, names_len, fn_len))
}

fn table_writer(table: Table, name_lens: Vec<uint>, fn_len: uint) {
    print!("┌");
    for &i in name_lens.iter() {
        print!("{}┬", "─".repeat(i + 2));
    }
    println!("{}┐", "─".repeat(fn_len + 1));
    for (i, &(ref names, ref fx)) in table.iter().enumerate() {
        print!("│");
        for nom in range(0, names.len()) {
            print!("{}{} │", " ".repeat(1 + name_lens[nom] - names[nom].len()), names[nom]);
        }

        let (start, middle, end, horiz) = if i == 0 {
            ('╞', '╪', '╡', "═")
        } else if i == table.len() - 1 {
            ('└', '┴', '┘', "─")
        } else {
            ('├', '┼', '┤', "─")
        };
        
        println!("{}{}│", " ".repeat(fn_len - fx.len() + 1), fx);
        print!("{}", start);
        for &i in name_lens.iter() {
            print!("{}{}", horiz.repeat(i + 2), middle);
        }
        println!("{}{}", horiz.repeat(fn_len + 1), end);
    }
}

pub fn table(args: &Vec<ArgType>, env: &mut Env) -> CalcResult {    
    if args.len() < 2 {
        return Err(BadNumberOfArgs("table".to_string(), "at least".to_string(), 2))
    }

    let (names, func) = try!(try!(args[0].desymbolize(env)).to_proc());
    if names.len() < 1 {
        return Err(BadArgType("At least one variable must be supplied".to_string()))
    }

    let fun_str = match args[0] {
        Atom(Symbol(ref x)) => x.clone(),
        _ => func.to_symbol(env)
    };

    if args.len() - 1 != names.len() {
        return Err(BadNumberOfArgs(func.to_string(), "only".to_string(), names.len()))
    }

    let mut lists: Vec<Vec<Lit>> = Vec::with_capacity(args.tail().len());
    for arg in args.tail().iter() {
        lists.push(try!(try!(arg.desymbolize(env)).to_vec()));
    }

    if lists.tail().iter().any(|x| x.len() != lists[0].len()) {
        return Err(BadArgType("Each list of arguments must be the same length".to_string()))
    }

    let (table, names_len, fn_len) = try!(make_table(lists, names, func, fun_str, env));

    table_writer(table, names_len, fn_len);
    
    Ok(Atom(Void))
}

pub fn table_from_matrix(args: &Vec<ArgType>, env: &mut Env) -> CalcResult {
    if args.len() != 2 {
        return Err(BadNumberOfArgs("table-from-matrix".to_string(), "only".to_string(), 2))
    }

    let matrix = match try!(args[0].desymbolize(env)) {
        Matrix(x) => x.clone(),
        x => return Err(BadArgType(format!("Expected matrix but found {}", x)))
    };

    if matrix.cols() < 2 {
        return Err(BadArgType("Expeted at least one variable".to_string()))
    }

    let (names, func) = try!(try!(args[1].desymbolize(env)).to_proc());
    let fun_str = match args[1] {
        Atom(Symbol(ref x)) => x.clone(),
        _ => func.to_symbol(env)
    };

    let matrix_vars = matrix.cols() - 1;
    if names.len() != matrix_vars {
        return Err(BadArgType(format!("Expected {} args but found {}", 
                                      matrix_vars, names.len())))
    }

    let mut table: Table = Vec::with_capacity(matrix.rows() + 1);
    let mut name_lens = Vec::from_elem(matrix_vars, 0u);
    let mut fn_len = fun_str.len();

    table.push((names, fun_str));
    for row in range(0, matrix.rows()) {
        let mut t_names: Vec<String> = matrix.get_row(row).map(|x| x.to_string()).collect();

        let val = t_names.pop().unwrap();

        if val.len() > fn_len {
            fn_len = t_names.last().unwrap().len();
        }

        for nom in range(0, t_names.len()) {
            if t_names[nom].len() > name_lens[nom] {
                *name_lens.get_mut(nom) = t_names[nom].len();
            }
        }        
        
        table.push((t_names, val))
    }

    println!("{}", table);
    println!("{} {}", name_lens, fn_len);
    table_writer(table, name_lens, fn_len);
    Ok(Atom(Void))
}

/*        
pub fn merge<T: PartialOrd>(left: Vec<T>, right: Vec<T>) -> Vec<T> {
    struct OrderedIterator<T, A, B> {
        a: iter::Peekable<T, A>,
        b: iter::Peekable<T, B>,
    }
    impl<T: PartialOrd, A: Iterator<T>, B: Iterator<T>> Iterator<T> for OrderedIterator<T, A, B> {
        fn next(&mut self) -> Option<T> {
            if self.a.peek().is_none() || 
                self.b.peek().is_none() { self.a.next().or_else(|| self.b.next()) }
            else if self.a.peek() < self.b.peek() { self.a.next() }
            else { self.b.next() }
        }

        fn size_hint(&self) -> (uint, Option<uint>) {
            let (a_min, a_max) = self.a.size_hint();
            let (b_min, b_max) = self.b.size_hint();
            (cmp::max(a_min, b_min), cmp::max(a_max, b_max))
        }
    }

    (OrderedIterator{a:left.move_iter().peekable(), 
                     b: right.move_iter().peekable()}).collect()
}
*/

#[inline]
pub fn sort(args: &Vec<ArgType>, env: &mut Env) -> CalcResult {
    if args.len() != 1 {
        return Err(BadNumberOfArgs("Sort".to_string(), "only".to_string(), 1))
    }

    let mut list = try!(try!(args[0].desymbolize(env)).to_vec());

    list.sort();

    Ok(Atom(List(list)))
}

pub fn sort_by(args: &Vec<ArgType>, env: &mut Env) -> CalcResult {
    use self::types::operator;
    use self::types::operator::{Lt, Gt};
    use self::types::sexpr::BuiltIn;

    if args.len() != 2 {
        return Err(BadNumberOfArgs("sort-by".to_string(), "only".to_string(), 2))
    }

    let mut list = try!(try!(args[0].desymbolize(env)).to_vec());

    let order = match try!(try!(args[1].desymbolize(env)).to_proc()) {
        (_, procedure) => match procedure.expr_type { 
            BuiltIn(operator::Ordering(cmp)) => cmp,
            _ => return Err(BadArgType("Use only builtin".to_string()))
        }
    };

    match order {
        Lt => list.sort_by(|a, b| a.cmp(b)),
        Gt => list.sort_by(|a, b| b.cmp(a)),
        _ => return Err(BadArgType("Use only < and >".to_string()))
    }

    Ok(Atom(List(list)))
}
