extern crate types;

use self::types::record::*;
use self::types::literal::{Lit, Structure, Proto, Procedure, Void};
use super::{Atom, Args, BadNumberOfArgs, BadArgType, CalcResult, 
            Env, Environment, Evaluate};

pub fn record_ops(args: &Args, env: &mut Env, rop: RecordOps) -> CalcResult {
    match rop {
        MakeStruct => make_struct(args, env),
        SetFields => add_fields(args, env),
        SetMethods => add_methods(args, env),
        GetField => get_field(args, env), 
        CallMethod => call_method(args, env),
        DelField | DelMethod => del_attrib(args, env, rop),
        DefineRecord => define_record(args, env),
    }
}

pub fn list_to_field(mut list: Vec<Lit>, env: &mut Env) -> CalcResult<(String, Lit)> {
    if list.len() != 2 {
        return Err(BadArgType("Supply a name for the field, and value".to_string()))
    }

    let name = try!(list[0].to_sym_string());
    let data = try!(Atom(list.pop().unwrap()).desymbolize(env));

    Ok((name, data))
}

pub fn list_to_method(mut list: Vec<Lit>, env: &mut Env) -> CalcResult<(String, Procedure)> {
    if list.len() != 2 {
        return Err(BadArgType("Provide a name and function".to_string()))
    }

    let name = try!(list[0].to_sym_string());
    let procedure = try!(try!(Atom(list.pop().unwrap()).desymbolize(env)).to_procedure());

    Ok((name, procedure))
}

pub fn define_record(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() < 2 {
        return Err(BadNumberOfArgs("define-record".to_string(), "at least".to_string(), 2))
    } else if args.len() > 3 {
        return Err(BadNumberOfArgs("define-record".to_string(), "at most".to_string(), 3))
    }

    let mut proto = ProtoRecord::new(&try!(try!(args[0].arg_to_literal(env)).to_sym_string()));
    let field_list = try!(try!(args[1].desymbolize(env)).to_vec());
    let mut fields = Vec::with_capacity(field_list.len());

    for name in field_list.move_iter() {
        fields.push(try!(name.to_sym_string()));
    }

    proto.set_fields(&fields);

    if args.len() == 3 {
        let methods = try!(try!(args[2].desymbolize(env)).to_vec());
        for method_list in methods.move_iter() {
            let list = try!(try!(Atom(method_list).desymbolize(env)).to_vec());
            let (name, method) = try!(list_to_method(list, env));
            proto.set_method(&name, &method);
        }
    }

    env.symbols.insert(proto.name().clone(), Proto(proto));
    Ok(Atom(Void))
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

pub fn get_field(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() != 2 {
        return Err(BadNumberOfArgs("get-field".to_string(), "only".to_string(), 2))
    }

    let record = try!(try!(args[0].desymbolize(env)).to_structure());
    let field = try!(try!(args[1].arg_to_literal(env)).to_sym_string());
    Ok(Atom(try!(record.get_field(&field)).clone()))
}

pub fn call_method(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() < 2 {
        return Err(BadNumberOfArgs("call-method".to_string(), "at least".to_string(), 2))
    }

    let record = try!(try!(args[0].desymbolize(env)).to_structure());
    let method = try!(try!(args[1].arg_to_literal(env)).to_sym_string());

    let (params, fun) = try!(record.get_method(&method)).destruct();
    if params.len() + 2 != args.len() {
        return Err(BadNumberOfArgs(method, "only".to_string(), params.len()))
    }

    let mut child_env = Environment::new_frame(env);
    for (param, val) in params.iter().zip(args.slice_from(2).iter()) {
        child_env.symbols.insert(param.clone(), try!(val.desymbolize(env)));
    }

    for (name, field) in record.fields().iter() {
        child_env.symbols.insert(format!("{}.{}", record.name(), name), 
                                 field.clone());
    }

    fun.eval(&mut child_env)
}

pub fn del_attrib(args: &Args, env: &mut Env, rop: RecordOps) -> CalcResult {
    if args.len() != 2 {
        return Err(BadNumberOfArgs(rop.to_string(), "only".to_string(), 2))
    }

    let mut record = try!(try!(args[0].desymbolize(env)).to_structure());
    let name = try!(try!(args[1].arg_to_literal(env)).to_sym_string());

    match rop {
        DelField => try!(record.del_field(&name)),
        _ => try!(record.del_method(&name)),
    }

    Ok(Atom(Structure(record)))
}

pub fn make_struct(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() < 1 {
        return Err(BadNumberOfArgs("make-struct".to_string(), "at least".to_string(), 1))
    }

    let proto = try!(try!(args[0].desymbolize(env)).to_proto());
    let mut field_vals = Vec::with_capacity(args.tail().len());

    for arg in args.tail().iter() {
        field_vals.push(try!(arg.desymbolize(env)));
    }

    let record = try!(proto.to_record(field_vals.as_slice()));

    Ok(Atom(Structure(record)))
}
