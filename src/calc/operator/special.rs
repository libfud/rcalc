//! Special functions like table and plot points.

extern crate types;
extern crate image;

use std::io::File;
use self::types::Expr;
use self::types::literal::{Lit, Symbol, Void};
use self::image::GenericImage;
use super::super::{Evaluate, BadArgType, BadNumberOfArgs};
use super::{Args, Env, Environment, CalcResult, Atom};

type Lists = Vec<Vec<Lit>>;
pub type Table = Vec<(Vec<String>, String)>;

///takes a function, beginning for x axis, y axis, and how many units to draw, and filename
pub fn graph(args: &Args, env: &mut Env) -> CalcResult {
    use std::num;
    use std::iter::range_step;

    if args.len() < 8 {
        return Err(BadNumberOfArgs("graph".to_string(), "only".to_string(), 8))
    }

    let (names, func) = try!(try!(args[0].desymbolize(env)).to_proc());
    let (begin_x, begin_y) = (try!(args[1].desymbolize(env)), try!(args[2].desymbolize(env)));
    let (width, height) = (try!(args[3].desymbolize(env)), try!(args[4].desymbolize(env)));
    let (x_zoom, y_zoom) = (try!(args[5].desymbolize(env)), try!(args[6].desymbolize(env)));
    let filepath = try!(try!(args[7].arg_to_literal(env)).to_sym_string());

    if names.len() != 1 {
        return Err(BadArgType("No!".to_string()))
    }

    if [&width, &height, &x_zoom, &y_zoom].iter().any(|x| **x < num::zero() || 
                                                      x.is_integer() == Ok(false)) {
        return Err(BadArgType("No!".to_string()))
    }

    let imgx = 1024;
    let imgy = 1024;
    let mut imgbuf = self::image::ImageBuf::new(imgx, imgy);

    let one: Lit = num::one();
    let two = one + one;
    let eight = two * two * two;
    let step = x_zoom / eight;

    let mut child_env = Environment::new_frame(env);
    for x in range_step(begin_x.clone(), begin_x + width, step) {
        child_env.symbols.insert(names[0].clone(), x.clone());
        let result = try!(try!(func.eval(&mut child_env)).desymbolize(env));

        let pixel = image::Luma(255);
        let this_x = try!((x - begin_x).to_uint()) as u32;
        let this_y = try!((result - begin_y).to_uint()) as u32;
        if this_y < 1024 {
            imgbuf.put_pixel(this_x, 1024 - this_y, pixel);
        }
    }

    let fout = File::create(&Path::new(filepath.append(".png").as_slice())).unwrap();
    let _ = image::ImageLuma8(imgbuf).save(fout, image::PNG);

    Ok(Atom(Void))
}

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

pub fn table(args: &Args, env: &mut Env) -> CalcResult {    
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

pub fn table_from_matrix(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() != 2 {
        return Err(BadNumberOfArgs("table-from-matrix".to_string(), "only".to_string(), 2))
    }

    let matrix = try!(try!(args[0].desymbolize(env)).to_matrix());
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
