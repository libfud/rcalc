use std::collections::HashMap;
use std::{cmp, fmt, from_str};
use super::{BadArgType, CalcResult, UnboundArg};
use super::literal::{Lit, Procedure};

#[deriving(Clone, PartialEq, PartialOrd, Eq, Ord)]
pub enum Vis {
    Private,
    Public,
}

impl from_str::FromStr for Vis {
    #[inline]
    fn from_str(s: &str) -> Option<Vis> {
        match s {
            "public" => Some(Public),
            "private" => Some(Private),
            _ => None
        }
    }
}

impl fmt::Show for Vis {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match self {
            &Private => "private",
            &Public => "public"
        }));
        Ok(())
    }
}

#[deriving(Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct Field {
    visibility: Vis,
    data: Lit
}

impl fmt::Show for Field {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{} {}", self.visibility, self.data));
        Ok(())
    }
}

impl<'a> Field {
    #[inline]
    pub fn new_private(data: &Lit) -> Field {
        Field { visibility: Private, data: data.clone() }
    }

    #[inline]
    pub fn new_public(data: &Lit) -> Field {
        Field { visibility: Public, data: data.clone() }
    }

    #[inline]
    pub fn new(vis: Vis, data: Lit) -> Field {
        Field { visibility: vis, data: data }
    }

    #[inline]
    pub fn visibility(&self) -> Vis {
        self.visibility
    }

    #[inline]
    pub fn data(&'a self) -> &'a Lit {
        &self.data
    }

    #[inline]
    pub fn set_data(&mut self, new_data: &Lit) {
        self.data = new_data.clone();
    }

    #[inline]
    pub fn set_visibility(&mut self, new_vis: Vis) {
        self.visibility = new_vis;
    }
}

pub type FieldTable = HashMap<String, Field>;
pub type MethodTable = HashMap<String, Procedure>;

#[deriving(Clone)]
pub struct Record {
    name: String,
    fields: FieldTable,
    methods: MethodTable
}

impl fmt::Show for Record {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "record {}: ", self.name));

        if self.fields.is_empty() {
            try!(write!(fmt, "No fields, "));
        } else {
            try!(writeln!(fmt, ""));
            for (name, val) in self.fields.iter() {
                try!(write!(fmt, "{} field {}: ", val.visibility, name));
                try!(writeln!(fmt, "{}", match val.data {
                    super::literal::Matrix(ref x) => format!("\n{}", x),
                    ref x => x.to_string()
                }));
            }
        }

        if self.methods.is_empty() {
            try!(write!(fmt, "No methods"));
        } else {
            for (name, method) in self.methods.iter() {
                try!(writeln!(fmt, "method {}: {}", name, method));
            }
        }

        Ok(())
    }
}

impl<'a> Record {
    #[inline]
    pub fn new(name: &String) -> Record {
        Record { name: name.clone(), fields: HashMap::new(), methods: HashMap::new() }
    }

    #[inline]
    pub fn name(&'a self) -> &'a String {
        &self.name
    }

    #[inline]
    pub fn fields(&'a self) -> &'a FieldTable {
        &self.fields
    }

    #[inline]
    pub fn methods(&'a self) -> &'a MethodTable {
        &self.methods
    }

    #[inline]
    pub fn number_of_fields(&self) -> uint {
        self.fields.len()
    }

    #[inline]
    pub fn number_of_methods(&self) -> uint {
        self.methods.len()
    }

    #[inline]
    pub fn get_field(&'a self, field: &String) -> CalcResult<&'a Field> {
        match self.fields.find(field) {
            Some(x) => Ok(x),
            None => Err(UnboundArg(field.clone()))
        }
    }

    #[inline]
    pub fn field_to_lit(&self, field: &String) -> CalcResult<Lit> {
        match self.fields.find(field) {
            Some(f) => match f.visibility {
                Private => Err(BadArgType(format!("{} is private!", field))),
                Public => Ok(f.data.clone())
            },
            None => Err(UnboundArg(field.clone()))
        }
    }

    #[inline]
    pub fn privates_to_lit(&self, field: &String) -> CalcResult<Lit> {
        match self.fields.find(field) {
            Some(f) => Ok(f.data.clone()),
            None => Err(UnboundArg(field.clone()))
        }
    }

    #[inline]
    pub fn get_method(&'a self, method: &String) -> CalcResult<&'a Procedure> {
        match self.methods.find(method) {
            Some(x) => Ok(x),
            None => Err(UnboundArg(method.clone()))
        }
    }

    #[inline]
    pub fn method_to_proc(&self, method: &String) -> CalcResult<Lit> {
        match self.methods.find(method) {
            Some(f) => Ok(f.to_lit()),
            None => Err(UnboundArg(method.clone()))
        }
    }

    #[inline]
    pub fn set_name(&mut self, name: &String) {
        self.name = name.clone();
    }

    #[inline]
    pub fn set_field(&mut self, name: &String, val: &Field) {
        self.fields.insert(name.clone(), val.clone());
    }

    #[inline]
    pub fn del_field(&mut self, name: &String) -> CalcResult<()> {
        if self.fields.remove(name) {
            Ok(())
        } else {
            Err(UnboundArg(name.clone()))
        }
    }

    #[inline]
    pub fn set_fields(&mut self, fields: &FieldTable) {
        self.fields = fields.clone();
    }

    #[inline]
    pub fn set_method(&mut self, name: &String, method: &Procedure) {
        self.methods.insert(name.clone(), method.clone());
    }

    #[inline]
    pub fn del_method(&mut self, name: &String) -> CalcResult<()> {
        if self.methods.remove(name) {
            Ok(())
        } else {
            Err(UnboundArg(name.clone()))
        }
    }

    #[inline]
    pub fn set_methods(&mut self, methods: &MethodTable) {
        self.methods = methods.clone();
    }
}

impl PartialEq for Record {
    #[inline]
    fn eq(&self, other: &Record) -> bool {
        self.name == other.name
    }

    #[inline]
    fn ne(&self, other: &Record) -> bool {
        self.name == other.name
    }
}

impl PartialOrd for Record {
    #[inline]
    fn partial_cmp(&self, other: &Record) -> Option<cmp::Ordering> {
        self.name.partial_cmp(&other.name)
    }
}

impl Eq for Record { }
impl Ord for Record {
    #[inline]
    fn cmp(&self, other: &Record) -> cmp::Ordering {
        self.name.cmp(&other.name)
    }
}


#[deriving(Clone, PartialOrd, PartialEq, Eq, Ord)]
pub enum RecordOps {
    MakeStruct,
    SetFields,
    SetMethods,
    SetName,
    GetField,
    CallMethod,
    DelField,
    DelMethod
}

impl fmt::Show for RecordOps {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match *self {
            MakeStruct => "make-struct",
            SetFields => "set-fields",
            SetMethods => "set-methods" ,
            SetName => "set-name",
            GetField => "get-field",
            CallMethod => "call-method",
            DelField => "del-field",
            DelMethod => "del-method"}));
        Ok(())
    }
}

impl from_str::FromStr for RecordOps {
    #[inline]
    fn from_str(s: &str) -> Option<RecordOps> {
        match s {
            "make-struct" => Some(MakeStruct),
            "set-fields"  => Some(SetFields),
            "set-methods" => Some(SetMethods),
            "set-name"    => Some(SetName),
            "get-field"   => Some(GetField),
            "call-method" => Some(CallMethod),
            "del-field"   => Some(DelField),
            "del-method"  => Some(DelMethod),
            _ => None
        }
    }
}
