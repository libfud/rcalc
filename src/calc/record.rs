extern crate types;

use self::types::record::*;
use self::types::literal::{Lit, Structure, Procedure, Void};
use super::{Atom, ArgType, BadNumberOfArgs, BadArgType, CalcResult, 
            Environment, Evaluate};

pub type Args = Vec<ArgType>;
pub type Env = Environment;

pub fn record_ops(args: &Args, env: &mut Env, rop: RecordOps) -> CalcResult {
    match rop {
        MakeStruct => make_struct(args, env),
        AddFields  => add_fields(args, env), 
        AddMethods => add_methods(args, env), /*
        SetFields,
        SetMethods,
        SetName,
        GetField,
        CallMethod,
        DelField,
        DelMethod */
        _ => Ok(Atom(Void))
    }
}

pub fn make_struct(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() != 1 {
        return Err(BadNumberOfArgs("make-struct".to_string(), "only".to_string(), 1))
    }

    let record = Record::new(&try!(try!(args[0].arg_to_literal(env)).to_sym_string()));
    Ok(Atom(Structure(record)))
}

pub fn list_to_field(mut list: Vec<Lit>, env: &mut Env) -> CalcResult<(String, Field)> {
    if list.len() != 3 {
        return Err(BadArgType(
            "Supply a name for the field, its visibility, and value".to_string()))
    }

    let vis = match from_str::<Vis>(try!(list[0].to_sym_string()).as_slice()) {
        Some(x) => x,
        None => return Err(BadArgType("Must give visibility of field".to_string()))
    };
    let name = try!(list[1].to_sym_string());
    let data = try!(Atom(list.pop().unwrap()).desymbolize(env));

    let field = Field::new(vis, data);

    Ok((name, field))
}

pub fn list_to_method(mut list: Vec<Lit>, env: &mut Env) -> CalcResult<(String, Procedure)> {
    if list.len() != 2 {
        return Err(BadArgType("Provide a name and function".to_string()))
    }

    let name = try!(list[0].to_sym_string());
    let procedure = try!(try!(Atom(list.pop().unwrap()).desymbolize(env)).to_procedure());

    Ok((name, procedure))
}

pub fn add_fields(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() < 2 {
        return Err(BadNumberOfArgs("add-fields".to_string(), "at least".to_string(), 2))
    }

    let mut record = try!(try!(args[0].desymbolize(env)).to_structure());

    for arg in args.tail().iter() {
        let (name, field) = try!(list_to_field(try!(try!(arg.desymbolize(env)).to_vec()), env));
        record.set_field(&name, &field);
    }

    Ok(Atom(Structure(record)))
}

pub fn add_methods(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() < 2 {
        return Err(BadNumberOfArgs("add-methods".to_string(), "at least".to_string(), 2))
    }

    let mut record = try!(try!(args[0].desymbolize(env)).to_structure());

    for arg in args.tail().iter() {
        let (name, method) = try!(list_to_method(try!(try!(arg.desymbolize(env)).to_vec()), env));
        record.set_method(&name, &method);
    }

    Ok(Atom(Structure(record)))
}
