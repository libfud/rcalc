//! Special functions like table and plot points.

extern crate types;
//extern crate image;

//use std::fs::File;
use self::types::{Args, Expr, Env, Environment, CalcResult};
use self::types::literal::{Lit};
use self::types::LiteralType::{Symbol, Void};
use self::types::ErrorKind::{BadArgType, BadNumberOfArgs};
use self::types::sexpr::ArgType::Atom;
//use self::image::GenericImage;
use super::super::Evaluate;

type Lists = Vec<Vec<Lit>>;
pub type Table = Vec<(Vec<String>, String)>;

///takes a function, beginning for x axis, y axis, and how many units to draw, and filename
pub fn graph(_args: &Args, _env: &mut Env) -> CalcResult {
/*    use std::num;
    use std::iter::{range_step};

    if args.len() < 8 {
        return Err(BadNumberOfArgs("graph".to_string(), "at least".to_string(), 8))
    } else if args.len() > 10 {
        return Err(BadNumberOfArgs("graph".to_string(), "at most".to_string(), 10))
    }

    let (names, func) = try!(try!(args[0].desymbolize(env)).to_proc());
    let (begin_x, begin_y) = (try!(args[1].desymbolize(env)), try!(args[2].desymbolize(env)));
    let (width, height) = (try!(args[3].desymbolize(env)), try!(args[4].desymbolize(env)));
    let step = try!(args[5].desymbolize(env));
    let zoom = try!(args[6].desymbolize(env));
    let filepath = try!(try!(args[7].arg_to_literal(env)).to_sym_string());
    let imgx = if args.len() > 8 {
        (try!(try!(args[8].desymbolize(env)).to_usize()) as u32)
    } else {
        1024u32
    };
    let imgy = if args.len() == 10 {
        (try!(try!(args[9].desymbolize(env)).to_usize()) as u32)
    } else {
        1024u32
    };

    if names.len() != 1 {
        return Err(BadArgType("Only graph functions taking one argument".to_string()))
    }

    if [&width, &height].iter().any(|x| **x < num::zero()) {
        return Err(BadArgType("Width and height must be positive.".to_string()))
    }

    let mut imgbuf = self::image::ImageBuf::new(imgx, imgy);
    let mut child_env = Environment::new_frame(env);
    let mut counter = 0;
    for x in range_step(begin_x.clone(), begin_x + width, step) {
        child_env.symbols.insert(names[0].clone(), x.clone());
        let result = try!(try!(func.eval(&mut child_env)).desymbolize(env)) * zoom;

        if result > begin_y && result < begin_y + height {
            let pixel = image::Luma(255); 
            let y = try!((begin_y + height - result).to_usize()) as u32;
            imgbuf.put_pixel(counter, y, pixel);
        }
        counter += 1;
    }

    let fout = File::create(&Path::new(format!("{}{}{}","images/",filepath,".png").as_slice())).unwrap();
    let _ = image::ImageLuma8(imgbuf).save(fout, image::PNG);
*/
    println!("Sorry, nonfunctional right now. Wait for rust-image to stabilize.");
    Ok(Atom(Void))
}

fn make_table(lists: Lists, names: Vec<String>, func: Expr, fun_str: String,
              env: &mut Env) -> CalcResult<(Table, Vec<usize>, usize)> {
    if lists.len() < 1 {
        panic!("make-table requires at least one list of variables")
    }

    let mut names_len = Vec::with_capacity(names.len());
    let (mut table, mut fn_len) = (Vec::new(), fun_str.len());
    table.push((names.clone(), fun_str));

    for column in (0.. lists[0].len()) {
        let mut child_env = Environment::new_frame(env);

        let values: Vec<Lit> = lists.iter().map(|x| x[column].clone()).collect();
        let mut t_names: Vec<String> = Vec::with_capacity(values.len());

        for val in (0 .. values.len()) {
            let name = values[val].to_string();
            if name.len() > names_len[val] {
                *names_len.get_mut(val).unwrap() = name.len();
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

fn table_writer(_table: Table, _name_lens: Vec<usize>, _fn_len: usize) {
/*
    print!("┌");
    for &i in name_lens.iter() {
        print!("{}┬", "─".repeat(i + 2));
    }
    println!("{}┐", "─".repeat(fn_len + 1));
    for (i, &(ref names, ref fx)) in table.iter().enumerate() {
        print!("│");
        for nom in (0.. names.len()) {
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
*/
    println!("it's screwed up.");
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

pub fn table_from_matrix(_args: &Args, _env: &mut Env) -> CalcResult {
/*
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
    let mut name_lens = Vec::with_capacity(matrix_vars, 0u);
    let mut fn_len = fun_str.len();

    table.push((names, fun_str));
    for row in (0.. matrix.rows()) {
        let mut t_names: Vec<String> = matrix.get_row(row).map(|x| x.to_string()).collect();
        let val = t_names.pop().unwrap();

        if val.len() > fn_len {
            fn_len = t_names.last().unwrap().len();
        }

        for nom in (0.. t_names.len()) {
            if t_names[nom].len() > name_lens[nom] {
                name_lens.get_mut(nom) = t_names[nom].len();
            }
        }        
        
        table.push((t_names, val))
    }
*/
//    println!("{}", table);
//    println!("{} {}", name_lens, fn_len);
    println!("It's screwed up.");
//    table_writer(table, name_lens, fn_len);
    Ok(Atom(Void))
}
