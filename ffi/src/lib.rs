mod translate;
use crate::translate::ToRustCode;
use ast::ctx::Ctx;
use ast::err::{Error, Result};
use ast::{Debruijn, Type, Value, _Ident, _Program, _Type, _Value, _Vec};
use libloading::Library;
use rand::{distributions::Alphanumeric, Rng};
use std::fs;
use std::path::Path;
use std::process::Command;

pub struct FFI {
    module_path: String,
    code_path: String,
    lib: Library,
}

type DynResult<R> = std::result::Result<R, Box<dyn std::error::Error>>;

impl FFI {
    pub fn new(program: &_Program<Debruijn>, env: &Ctx<_Type<Debruijn>>) -> Result<Self> {
        let tmp = std::env::temp_dir().to_str().unwrap().to_string();
        let id: String = rand::thread_rng()
            .sample_iter(&Alphanumeric)
            .take(24)
            .map(char::from)
            .collect();
        let module_path = format!("{}/{}.module", tmp, id);
        let code_path = format!("{}/{}.rs", tmp, id);

        fs::write(&code_path, program.to_rust(env)?)
            .map_err(|e| Error::new(format!("failed to write module {}", e)))?;
        if !Command::new("rustc")
            .arg("--crate-type")
            .arg("dylib")
            .arg("-A")
            .arg("warnings")
            .arg("-o")
            .arg(&module_path)
            .arg(&code_path)
            .status()
            .map_err(|e| {
                Error::new(format!("failed to compile {}", e)).help("rustc needs to be installed")
            })?
            .success()
        {
            return Err(Error::new(format!(
                "rust error in {}.foo",
                program.tag.0.join("/")
            )));
        }

        Ok(FFI {
            module_path,
            code_path,
            lib: unsafe {
                Library::new(&format!("{}/{}.module", tmp, id))
                    .map_err(|e| Error::new(format!("failed to load module {}", e)))?
            },
        })
    }

    fn _call(
        &self,
        function: &_Ident<Debruijn>,
        args: &_Vec<Debruijn, (_Value<Debruijn>, _Type<Debruijn>)>,
        ty: &_Type<Debruijn>,
    ) -> DynResult<_Value<Debruijn>> {
        Ok(function.set(match (args.it().len(), ty.it()) {
            (0, Type::Unit) => {
                self.call0::<()>(function.it())?;
                Value::Unit
            }
            (0, Type::Bool) => Value::Bool(self.call0(function.it())?),
            (0, Type::Int) => Value::Int(self.call0(function.it())?),
            (0, Type::Str) => Value::Str(self.call0(function.it())?),
            (1, Type::Unit) => {
                call1!(self, args, function);
                Value::Unit
            }
            (1, Type::Bool) => Value::Bool(call1!(self, args, function)),
            (1, Type::Int) => Value::Int(call1!(self, args, function)),
            (1, Type::Str) => Value::Str(call1!(self, args, function)),
            (2, Type::Unit) => {
                call2!(self, args, function);
                Value::Unit
            }
            (2, Type::Bool) => Value::Bool(call2!(self, args, function)),
            (2, Type::Int) => Value::Int(call2!(self, args, function)),
            (2, Type::Str) => Value::Str(call2!(self, args, function)),
            _ => unimplemented!(),
        }))
    }

    pub fn call(
        &self,
        function: &_Ident<Debruijn>,
        args: &_Vec<Debruijn, (_Value<Debruijn>, _Type<Debruijn>)>,
        ty: &_Type<Debruijn>,
    ) -> Result<_Value<Debruijn>> {
        self._call(function, args, ty)
            .map_err(|e| Error::new("ffi error").label(function, format!("{}", e)))
    }
    fn call0<R>(&self, function: &String) -> DynResult<R> {
        unsafe {
            (self
                .lib
                .get::<unsafe fn() -> DynResult<R>>(function.as_bytes())?)()
        }
    }
    fn call1<T1, R>(&self, function: &String, t1: T1) -> DynResult<R> {
        unsafe {
            (self
                .lib
                .get::<unsafe fn(T1) -> DynResult<R>>(function.as_bytes())?)(t1)
        }
    }
    fn call2<T1, T2, R>(&self, function: &String, t1: T1, t2: T2) -> DynResult<R> {
        unsafe {
            (self
                .lib
                .get::<unsafe fn(T1, T2) -> DynResult<R>>(function.as_bytes())?)(t1, t2)
        }
    }
}

impl Drop for FFI {
    fn drop(&mut self) {
        let src = Path::new(&self.code_path);
        if src.exists() {
            fs::remove_file(src).unwrap()
        }
        let lib = Path::new(&self.module_path);
        if lib.exists() {
            fs::remove_file(lib).unwrap()
        }
    }
}

#[macro_export]
macro_rules! call1 {
    ($self:ident, $args:ident, $function:ident) => {
        match $args.it()[0].0.it() {
            Value::Unit => $self.call1($function.it(), ())?,
            Value::Bool(b) => $self.call1($function.it(), b.clone())?,
            Value::Int(i) => $self.call1($function.it(), i.clone())?,
            Value::Str(s) => $self.call1($function.it(), s.clone())?,
            _ => unimplemented!(),
        }
    };
}

#[macro_export]
macro_rules! call2 {
    ($self:ident, $args:ident, $function:ident) => {
        match ($args.it()[0].0.it(), $args.it()[1].0.it()) {
            (Value::Unit, Value::Unit) => $self.call2($function.it(), (), ())?,
            (Value::Unit, Value::Bool(b)) => $self.call2($function.it(), (), b.clone())?,
            (Value::Unit, Value::Int(i)) => $self.call2($function.it(), (), i.clone())?,
            (Value::Unit, Value::Str(s)) => $self.call2($function.it(), (), s.clone())?,
            (Value::Bool(b), Value::Unit) => $self.call2($function.it(), b.clone(), ())?,
            (Value::Bool(b), Value::Bool(b2)) => {
                $self.call2($function.it(), b.clone(), b2.clone())?
            }
            (Value::Bool(b), Value::Int(i)) => $self.call2($function.it(), b.clone(), i.clone())?,
            (Value::Bool(b), Value::Str(s)) => $self.call2($function.it(), b.clone(), s.clone())?,
            (Value::Int(i), Value::Unit) => $self.call2($function.it(), i.clone(), ())?,
            (Value::Int(i), Value::Bool(b)) => $self.call2($function.it(), i.clone(), b.clone())?,
            (Value::Int(i), Value::Int(i2)) => $self.call2($function.it(), i.clone(), i2.clone())?,
            (Value::Int(i), Value::Str(s)) => $self.call2($function.it(), i.clone(), s.clone())?,
            (Value::Str(s), Value::Unit) => $self.call2($function.it(), s.clone(), ())?,
            (Value::Str(s), Value::Bool(b)) => $self.call2($function.it(), s.clone(), b.clone())?,
            (Value::Str(s), Value::Int(i)) => $self.call2($function.it(), s.clone(), i.clone())?,
            (Value::Str(s), Value::Str(s2)) => {
                $self.call2($function.it(), s.clone(), s2.clone())?
            }
            _ => unimplemented!(),
        }
    };
}
