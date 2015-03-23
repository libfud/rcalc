use std::collections::HashMap;
use std::{cmp, fmt};
use std::str::FromStr;
use super::ErrorKind::UnboundArg;
use super::CalcResult;
use super::literal::{Lit, Procedure};

pub type FieldTable = HashMap<String, Lit>;
pub type MethodTable = HashMap<String, Procedure>;

#[derive(Debug, Clone)]
pub struct Record {
    name: String,
    fields: FieldTable,
    methods: MethodTable
}

impl fmt::Display for Record {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "record {}: ", self.name));

        if self.fields.is_empty() {
            try!(write!(fmt, "No fields, "));
        } else {
            try!(writeln!(fmt, ""));
            for (name, val) in self.fields.iter() {
                try!(write!(fmt, "field {}: ", name));
                try!(writeln!(fmt, "{}", match val {
                    &super::literal::LiteralType::Matrix(ref x) => format!("\n{}", x),
                    x => x.to_string()
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
    pub fn number_of_fields(&self) -> usize {
        self.fields.len()
    }

    #[inline]
    pub fn number_of_methods(&self) -> usize {
        self.methods.len()
    }

    #[inline]
    pub fn get_field(&'a self, field: &String) -> CalcResult<&'a Lit> {
        match self.fields.get(field) {
            Some(x) => Ok(x),
            None => Err(UnboundArg(field.clone()))
        }
    }

    #[inline]
    pub fn get_method(&'a self, method: &String) -> CalcResult<&'a Procedure> {
        match self.methods.get(method) {
            Some(x) => Ok(x),
            None => Err(UnboundArg(method.clone()))
        }
    }

    #[inline]
    pub fn method_to_proc(&self, method: &String) -> CalcResult<Lit> {
        match self.methods.get(method) {
            Some(f) => Ok(f.to_lit()),
            None => Err(UnboundArg(method.clone()))
        }
    }

    #[inline]
    pub fn set_name(&mut self, name: &String) {
        self.name = name.clone();
    }

    #[inline]
    pub fn set_field(&mut self, name: &String, val: &Lit) {
        self.fields.insert(name.clone(), val.clone());
    }

    #[inline]
    pub fn get_mut_field(&'a mut self, field: &String) -> CalcResult<&'a mut Lit> {
        match self.fields.get_mut(field) {
            Some(x) => Ok(x),
            None => Err(UnboundArg(field.clone()))
        }
    }

    #[inline]
    pub fn del_field(&mut self, name: &String) -> CalcResult<()> {
        match self.fields.remove(name) {
            Some(_) => Ok(()),
            None => Err(UnboundArg(name.clone()))
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
        match self.methods.remove(name) {
            Some(_) => Ok(()),
            None => Err(UnboundArg(name.clone()))
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

#[derive(Clone, Debug)]
pub struct ProtoRecord {
    name: String,
    fields: Vec<String>,
    methods: MethodTable
}

impl<'a> ProtoRecord {
    #[inline]
    pub fn new(name: &String) -> ProtoRecord {
        ProtoRecord { name: name.clone(), fields: Vec::new(), methods: HashMap::new() }
    }

    pub fn to_record(&self, field_vals: &[Lit]) -> CalcResult<Record> {
        if field_vals.len() != self.fields.len() {
            return Err(super::ErrorKind::BadArgType("Mismatched fields".to_string()))
        }

        let mut fields = HashMap::new();
        for (name, val) in self.fields.iter().zip(field_vals.iter()) {
            fields.insert(name.clone(), val.clone());
        }

        let mut record = Record::new(&self.name);
        record.set_fields(&fields);
        record.set_methods(&self.methods);
        Ok(record)
    }        

    #[inline]
    pub fn name(&'a self) -> &'a String {
        &self.name
    }

    #[inline]
    pub fn fields(&'a self) -> &'a Vec<String>  {
        &self.fields
    }

    #[inline]
    pub fn methods(&'a self) -> &'a MethodTable {
        &self.methods
    }

    #[inline]
    pub fn get_method(&'a self, method: &String) -> CalcResult<&'a Procedure> {
        match self.methods.get(method) {
            Some(x) => Ok(x),
            None => Err(UnboundArg(method.clone()))
        }
    }

    #[inline]
    pub fn method_to_proc(&self, method: &String) -> CalcResult<Lit> {
        match self.methods.get(method) {
            Some(f) => Ok(f.to_lit()),
            None => Err(UnboundArg(method.clone()))
        }
    }

    #[inline]
    pub fn set_name(&mut self, name: &String) {
        self.name = name.clone();
    }

    #[inline]
    pub fn set_fields(&mut self, fields: &Vec<String>) {
        self.fields = fields.clone();
    }

    #[inline]
    pub fn set_method(&mut self, name: &String, method: &Procedure) {
        self.methods.insert(name.clone(), method.clone());
    }

    #[inline]
    pub fn del_method(&mut self, name: &String) -> CalcResult<()> {
        match self.methods.remove(name) {
            Some(_) => Ok(()),
            None => Err(UnboundArg(name.clone()))
        }
    }

    #[inline]
    pub fn set_methods(&mut self, methods: &MethodTable) {
        self.methods = methods.clone();
    }
}


impl fmt::Display for ProtoRecord {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "protorecord {}: ", self.name));

        if self.fields.is_empty() {
            try!(write!(fmt, "No fields, "));
        } else {
            try!(writeln!(fmt, ""));
            for name in self.fields.iter() {
                try!(write!(fmt, "field {}: ", name));
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


impl PartialEq for ProtoRecord {
    #[inline]
    fn eq(&self, other: &ProtoRecord) -> bool {
        self.name == other.name
    }

    #[inline]
    fn ne(&self, other: &ProtoRecord) -> bool {
        self.name == other.name
    }
}

impl PartialOrd for ProtoRecord {
    #[inline]
    fn partial_cmp(&self, other: &ProtoRecord) -> Option<cmp::Ordering> {
        self.name.partial_cmp(&other.name)
    }
}

impl Eq for ProtoRecord { }
impl Ord for ProtoRecord {
    #[inline]
    fn cmp(&self, other: &ProtoRecord) -> cmp::Ordering {
        self.name.cmp(&other.name)
    }
}

#[derive(Debug, Clone, PartialOrd, PartialEq, Eq, Ord)]
pub enum RecordOps {
    MakeStruct,
    SetFields,
    SetMethods,
    GetField,
    CallMethod,
    DelField,
    DelMethod,
    DefineRecord,
}

impl fmt::Display for RecordOps {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match *self {
            RecordOps::MakeStruct => "make-struct",
            RecordOps::SetFields => "set-fields",
            RecordOps::SetMethods => "set-methods" ,
            RecordOps::GetField => "get-field",
            RecordOps::CallMethod => "call-method",
            RecordOps::DelField => "del-field",
            RecordOps::DelMethod => "del-method",
            RecordOps::DefineRecord => "define-record",}));
        Ok(())
    }
}

impl FromStr for RecordOps {
    #[inline]
    type Err = ();
    fn from_str(s: &str) -> Result<RecordOps,()> {
        match s {
            "make-struct" => Ok(RecordOps::MakeStruct),
            "set-fields"  => Ok(RecordOps::SetFields),
            "set-methods" => Ok(RecordOps::SetMethods),
            "get-field"   => Ok(RecordOps::GetField),
            "call-method" => Ok(RecordOps::CallMethod),
            "del-field"   => Ok(RecordOps::DelField),
            "del-method"  => Ok(RecordOps::DelMethod),
            "define-record" => Ok(RecordOps::DefineRecord),
            _ => Err(())
        }
    }
}
