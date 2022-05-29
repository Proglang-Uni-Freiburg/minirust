mod translate;
use crate::translate::ToRustCode;
use ast::ctx::Ctx;
use ast::err::{Error, Result};
use libloading::Library;
use rand::{distributions::Alphanumeric, Rng};
use std::fs;
use std::process::Command;

ast::def_ast_types! {
    type => Debruijn,
    prefix => ast
}

pub struct FFI {
    id: String,
    lib: Library,
}

type DynResult<R> = std::result::Result<R, Box<dyn std::error::Error>>;

impl FFI {
    pub fn new(program: &Program, env: &Ctx<Type>) -> Result<Self> {
        let id: String = rand::thread_rng()
            .sample_iter(&Alphanumeric)
            .take(24)
            .map(char::from)
            .collect();
        let module = Self::build(program, env, &id)?;
        Ok(FFI {
            id,
            lib: unsafe {
                Library::new(module)
                    .map_err(|e| Error::new(format!("failed to load module {}", e)))?
            },
        })
    }

    fn build(program: &Program, env: &Ctx<Type>, id: &String) -> Result<String> {
        let tmp = std::env::temp_dir().to_str().unwrap().to_string();
        let module_path = format!("{}/{}.module", tmp, id);
        let code_path = format!("{}/{}.rs", tmp, id);

        fs::write(&code_path, program.to_rust(env)?)
            .map_err(|e| Error::new(format!("failed to write module {}", e)))?;

        if !Command::new("rustc")
            .arg("--crate-type")
            .arg("dylib")
            // .arg("-A")
            // .arg("warnings")
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
                "rust error in {}.mrs",
                program.tag.0.join("/")
            )));
        }
        Ok(module_path)
    }

    pub fn call(&self, function: &Ident, args: &Vec<(Value, Type)>, ret: &Type) -> Result<Value> {
        self._call(function, args, ret)
            .map_err(|e| Error::new("ffi error").label(function, format!("{}", e)))
    }

    fn call0<R>(&self, function: &str) -> DynResult<R> {
        unsafe {
            (self
                .lib
                .get::<unsafe fn() -> DynResult<R>>(function.as_bytes())?)()
        }
    }
    fn call1<T1, R>(&self, function: &str, t1: T1) -> DynResult<R> {
        unsafe {
            (self
                .lib
                .get::<unsafe fn(T1) -> DynResult<R>>(function.as_bytes())?)(t1)
        }
    }
    fn call2<T1, T2, R>(&self, function: &str, t1: T1, t2: T2) -> DynResult<R> {
        unsafe {
            (self
                .lib
                .get::<unsafe fn(T1, T2) -> DynResult<R>>(function.as_bytes())?)(t1, t2)
        }
    }

    fn _call(&self, function: &Ident, args: &Vec<(Value, Type)>, ty: &Type) -> DynResult<Value> {
        Ok(function.to(match (args.as_ref().len(), ty.as_ref()) {
            (0, ast::Type::Unit) => {
                self.call0::<()>(function.as_ref())?;
                ast::Value::Unit
            }
            (0, ast::Type::Bool) => ast::Value::Bool(self.call0(function.as_ref())?),
            (0, ast::Type::Int) => ast::Value::Int(self.call0(function.as_ref())?),
            (0, ast::Type::Str) => ast::Value::Str(self.call0(function.as_ref())?),
            (1, ast::Type::Unit) => {
                call1!(self, args, function);
                ast::Value::Unit
            }
            (1, ast::Type::Bool) => ast::Value::Bool(call1!(self, args, function)),
            (1, ast::Type::Int) => ast::Value::Int(call1!(self, args, function)),
            (1, ast::Type::Str) => ast::Value::Str(call1!(self, args, function)),
            (2, ast::Type::Unit) => {
                call2!(self, args, function);
                ast::Value::Unit
            }
            (2, ast::Type::Bool) => ast::Value::Bool(call2!(self, args, function)),
            (2, ast::Type::Int) => ast::Value::Int(call2!(self, args, function)),
            (2, ast::Type::Str) => ast::Value::Str(call2!(self, args, function)),
            _ => unimplemented!(),
        }))
    }
}

impl Drop for FFI {
    fn drop(&mut self) {
        let tmp = std::env::temp_dir().to_str().unwrap().to_string();
        let code_path = format!("{}/{}.rs", &tmp, self.id);
        let module_path = format!("{}/{}.module", &tmp, self.id);
        let code = std::path::Path::new(&code_path);
        if code.exists() {
            fs::remove_file(code).unwrap()
        }
        let lib = std::path::Path::new(&module_path);
        if lib.exists() {
            fs::remove_file(lib).unwrap()
        }
    }
}

#[macro_export]
macro_rules! call1 {
    ($self:ident, $args:ident, $function:ident) => {
        match $args.as_ref()[0].0.as_ref() {
            ast::Value::Unit => $self.call1($function.as_ref(), ())?,
            ast::Value::Bool(b) => $self.call1($function.as_ref(), b.clone())?,
            ast::Value::Int(i) => $self.call1($function.as_ref(), i.clone())?,
            ast::Value::Str(s) => $self.call1($function.as_ref(), s.clone())?,
            _ => unimplemented!(),
        }
    };
}

#[macro_export]
macro_rules! call2 {
    ($self:ident, $args:ident, $function:ident) => {
        match ($args.as_ref()[0].0.as_ref(), $args.as_ref()[1].0.as_ref()) {
            (ast::Value::Unit, ast::Value::Unit) => $self.call2($function.as_ref(), (), ())?,
            (ast::Value::Unit, ast::Value::Bool(b)) => {
                $self.call2($function.as_ref(), (), b.clone())?
            }
            (ast::Value::Unit, ast::Value::Int(i)) => {
                $self.call2($function.as_ref(), (), i.clone())?
            }
            (ast::Value::Unit, ast::Value::Str(s)) => {
                $self.call2($function.as_ref(), (), s.clone())?
            }
            (ast::Value::Bool(b), ast::Value::Unit) => {
                $self.call2($function.as_ref(), b.clone(), ())?
            }
            (ast::Value::Bool(b), ast::Value::Bool(b2)) => {
                $self.call2($function.as_ref(), b.clone(), b2.clone())?
            }
            (ast::Value::Bool(b), ast::Value::Int(i)) => {
                $self.call2($function.as_ref(), b.clone(), i.clone())?
            }
            (ast::Value::Bool(b), ast::Value::Str(s)) => {
                $self.call2($function.as_ref(), b.clone(), s.clone())?
            }
            (ast::Value::Int(i), ast::Value::Unit) => {
                $self.call2($function.as_ref(), i.clone(), ())?
            }
            (ast::Value::Int(i), ast::Value::Bool(b)) => {
                $self.call2($function.as_ref(), i.clone(), b.clone())?
            }
            (ast::Value::Int(i), ast::Value::Int(i2)) => {
                $self.call2($function.as_ref(), i.clone(), i2.clone())?
            }
            (ast::Value::Int(i), ast::Value::Str(s)) => {
                $self.call2($function.as_ref(), i.clone(), s.clone())?
            }
            (ast::Value::Str(s), ast::Value::Unit) => {
                $self.call2($function.as_ref(), s.clone(), ())?
            }
            (ast::Value::Str(s), ast::Value::Bool(b)) => {
                $self.call2($function.as_ref(), s.clone(), b.clone())?
            }
            (ast::Value::Str(s), ast::Value::Int(i)) => {
                $self.call2($function.as_ref(), s.clone(), i.clone())?
            }
            (ast::Value::Str(s), ast::Value::Str(s2)) => {
                $self.call2($function.as_ref(), s.clone(), s2.clone())?
            }
            _ => unimplemented!(),
        }
    };
}
