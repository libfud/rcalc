//! Special functions like table and plot points.

extern crate types;

use self::types::literal::{BigNum, List, Matrix, Symbol, Void, LiteralType};
use super::super::{Expression, Evaluate};
use super::listops::proc_getter;
use super::super::{BadArgType, BadNumberOfArgs};
use super::{Environment, CalcResult, ArgType, Atom};
use std::{iter, cmp};

pub fn range_getter(arg: LiteralType) -> CalcResult<int> {
    match arg {
        BigNum(x) => Ok(x.to_integer().to_int().unwrap()),
        _ => Err(BadArgType("Range and step must be integers!".to_string()))
    }
}

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
    use std::iter::AdditiveIterator;

    /* Beginning pipe, pipe and two spaces for each variable, pipe and space for
     * each answer */
    let total_len = 1 + name_lens.iter().map(|x| *x + 3).sum() + fn_len + 2;

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

fn old_table_writer(table: Vec<(String, String)>, name_len: uint, fn_len: uint) {
    println!("┌{}┬{}┐", "─".repeat(name_len), "─".repeat(1 + fn_len));
    for (i, &(ref x, ref fx)) in table.iter().enumerate() {
        println!("│{a}{b}│{c}{d}│", a = x, b = " ".repeat(name_len - x.len()),
                 d = fx, c = " ".repeat(fn_len - fx.len() + 1));
        if i == 0 {
            println!("╞{}╪{}╡", "═".repeat(name_len), "═".repeat(1 + fn_len));
        } else if i == table.len() - 1 {
            println!("└{}┴{}┘", "─".repeat(name_len), "─".repeat(1 + fn_len));
        } else {
            println!("├{}┼{}┤", "─".repeat(name_len), "─".repeat(1 + fn_len));
        }
    }
}

pub fn table(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {    
    if args.len() < 2 {
        return Err(BadNumberOfArgs("table".to_string(), "at least".to_string(), 2))
    }

    let (names, func) = try!(proc_getter(args, env));
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
        match try!(arg.desymbolize(env)) {
            List(x) => lists.push(x),
            _ => return Err(BadArgType("Arguments to function given as lists.".to_string()))
        }
    }

    if lists.tail().iter().any(|x| x.len() != lists[0].len()) {
        return Err(BadArgType("Each list of arguments must be the same length".to_string()))
    }

    let (table, names_len, fn_len) = try!(make_table(lists, names, func, fun_str, env));

    table_writer(table, names_len, fn_len);
    
    Ok(Atom(Void))
}

pub fn table_from_matrix(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
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

    let (names, func) = try!(proc_getter(&args.tail().to_owned(), env));

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
    let mut fn_len = 0u;

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
        

pub fn insertion_sort<T: PartialOrd + Clone>(mut array: Vec<T>) -> Vec<T> {
    if array.len() <= 1 {
        return array.to_owned()
    }

    let mut i = 1u;
    while i < array.len() {
        let val = array[i].clone();
        let mut j = i - 1;
        while j + 1 != 0 && array[j] > val {
            array.as_mut_slice()[j + 1] = array[j].clone();
            j -= 1;
        }
        array.as_mut_slice()[j + 1] = val;
        i += 1;
    }

    array
}

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
            

pub fn merge_sort<T: PartialOrd + Clone>(array: Vec<T>, 
                                         min_size: uint) -> CalcResult<Vec<T>> {
    if min_size < 1 {
        return Err(BadArgType("0 is an invalid minimum size!".to_string()))
    }

    let length = array.len();
    if length <= min_size { 
        return Ok(insertion_sort(array))
    }

    let middle = length / 2;
    let mut left = Vec::from_slice(array.slice(0, middle));
    let mut right = Vec::from_slice(array.slice(middle, length));

    left = try!(merge_sort(left, min_size));
    right = try!(merge_sort(right, min_size));

    Ok(merge(left, right))
}
        
pub fn sort(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    if args.len() != 1 {
        return Err(BadNumberOfArgs("Sort".to_string(), "only".to_string(), 1))
    }

    let list = match try!(args[0].arg_to_literal(env)) {
        List(x) => x.clone(),
        _ => return Err(BadArgType("Cannot sort items which aren't in a list!".to_string()))
    };

    let answer = try!(merge_sort(list, 100));

    Ok(Atom(List(answer)))
}
