mod translate;
use crate::translate::ToRustCode;
use ast::ctx::Ctx;
use ast::err::{Error, Result};
use ast::{Debruijn, Type, Value, _Ident, _Program, _Type, _Value, _Vec};
use libloading::Library;
use rand::{distributions::Alphanumeric, Rng};
use std::fs;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::process::{Command};

pub struct FFI {
    module_path: String,
    code_path: String,
}

impl FFI {
    pub fn new(program: &_Program<Debruijn>, env: &Ctx<_Type<Debruijn>>) -> Result<Self> {
        let tmp = std::env::temp_dir().to_str().unwrap().to_string();
        let id: String = rand::thread_rng()
            .sample_iter(&Alphanumeric)
            .take(24)
            .map(char::from)
            .collect();
        let ffi = FFI {
            module_path: format!("{}/{}.module", tmp, id),
            code_path: format!("{}/{}.rs", tmp, id),
        };
        ffi.write(program.to_rust(env)?).map_err(|_| Error::new("failed to write ffi"))?;
        if !ffi.compile().map_err(|e| {
            Error::new(format!("failed to compile {}", e)).help("rustc needs to be installed")
        })? {
            return Err(Error::new(format!("rust error in {}.foo", program.tag.0.join("/"))))
        };
        Ok(ffi)
    }
    fn write(&self, code: String,) -> std::io::Result<()> {
        let mut file = File::create(&self.code_path)?;
        file.write_all(code.as_bytes())?;
        Ok(())
    }
    fn compile(&self) -> std::io::Result<bool> {
        Ok(Command::new("rustc")
            .arg("--crate-type")
            .arg("dylib")
            .arg("-A")
            .arg("warnings")
            .arg("-o")
            .arg(&self.module_path)
            .arg(&self.code_path)
            .status()?
            .success())
    }
    pub fn call(
        &self,
        function: &_Ident<Debruijn>,
        args: &_Vec<Debruijn, (_Value<Debruijn>, _Type<Debruijn>)>,
        ty: &_Type<Debruijn>,
    ) -> Result<_Value<Debruijn>> {
        Ok(function.set(match (args.it().len(), ty.it()) {
            (0, Type::Unit) => {
                self.call0::<()>(function.it()).unwrap();
                Value::Unit
            }
            (0, Type::Bool) => Value::Bool(self.call0(function.it()).unwrap()),
            (0, Type::Int) => Value::Bool(self.call0(function.it()).unwrap()),
            (0, Type::Str) => Value::Str(self.call0(function.it()).unwrap()),
            (1, Type::Unit) => {
                match args.it()[0].0.it() {
                    Value::Unit => self.call1::<(), ()>(function.it(), ()).unwrap(),
                    Value::Bool(b) => self.call1::<_, _>(function.it(), b.clone()).unwrap(),
                    Value::Int(i) => self.call1::<_, _>(function.it(), i.clone()).unwrap(),
                    Value::Str(s) => self.call1::<_, _>(function.it(), s.clone()).unwrap(),
                    _ => unimplemented!(),
                }
                Value::Unit
            }
            (1, Type::Bool) => Value::Bool(match args.it()[0].0.it() {
                Value::Unit => self.call1::<(), bool>(function.it(), ()).unwrap(),
                Value::Bool(b) => self.call1::<_, _>(function.it(), b.clone()).unwrap(),
                Value::Int(i) => self.call1::<_, _>(function.it(), i.clone()).unwrap(),
                Value::Str(s) => self.call1::<_, _>(function.it(), s.clone()).unwrap(),
                _ => unimplemented!(),
            }),
            (1, Type::Int) => Value::Int(match args.it()[0].0.it() {
                Value::Unit => self.call1::<(), _>(function.it(), ()).unwrap(),
                Value::Bool(b) => self.call1::<_, _>(function.it(), b.clone()).unwrap(),
                Value::Int(i) => self.call1::<_, _>(function.it(), i.clone()).unwrap(),
                Value::Str(s) => self.call1::<_, _>(function.it(), s.clone()).unwrap(),
                _ => unimplemented!(),
            }),
            (1, Type::Str) => Value::Str(match args.it()[0].0.it() {
                Value::Unit => self.call1::<(), _>(function.it(), ()).unwrap(),
                Value::Bool(b) => self.call1::<_, _>(function.it(), b.clone()).unwrap(),
                Value::Int(i) => self.call1::<_, _>(function.it(), i.clone()).unwrap(),
                Value::Str(s) => self.call1::<_, _>(function.it(), s.clone()).unwrap(),
                _ => unimplemented!(),
            }),
            _ => unimplemented!(),
        }))
    }
    fn call0<R>(&self, function: &String) -> std::result::Result<R, libloading::Error> {
        unsafe {
            let lib = Library::new(&self.module_path)?;
            Ok((lib.get::<unsafe fn() -> R>(function.as_bytes())?)())
        }
    }
    fn call1<T1, R>(&self, function: &String, t1: T1) -> std::result::Result<R, libloading::Error> {
        unsafe {
            let lib = Library::new(&self.module_path)?;
            Ok((lib.get::<unsafe fn(T1) -> R>(function.as_bytes())?)(t1))
        }
    }
    /* fn call2<T1, T2, R>(
        &self,
        function: &String,
        t1: T1,
        t2: T2,
    ) -> std::result::Result<R, libloading::Error> {
        unsafe {
            let lib = Library::new(&self.module_path)?;
            Ok((lib.get::<unsafe fn(T1, T2) -> R>(function.as_bytes())?)(
                t1, t2,
            ))
        }
    }
    fn call3<T1, T2, T3, R>(
        &self,
        function: &String,
        t1: T1,
        t2: T2,
        t3: T3,
    ) -> std::result::Result<R, libloading::Error> {
        unsafe {
            let lib = Library::new(&self.module_path)?;
            Ok((lib
                .get::<unsafe fn(T1, T2, T3) -> R>(function.as_bytes())?)(
                t1, t2, t3,
            ))
        }
    } */
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
