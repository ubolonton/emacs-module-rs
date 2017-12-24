extern crate libc;

use emacs_gen::*;
use std::ffi::CString;
use libc::ptrdiff_t;
use std::result;

// TODO: Replace .unwrap() calls with non-local error signaling to Emacs.

/// We assume that the C code in Emacs really treats it as an enum and doesn't return an undeclared
/// value, but we still need to safeguard against possible compatibility issue (Emacs may add more
/// statuses in the future). FIX: Use an enum, and check for compatibility on load.
pub type FuncallExit = emacs_funcall_exit;
pub const FUNCALL_EXIT_RETURN: FuncallExit = emacs_funcall_exit_emacs_funcall_exit_return;
pub const FUNCALL_EXIT_SIGNAL: FuncallExit = emacs_funcall_exit_emacs_funcall_exit_signal;
pub const FUNCALL_EXIT_THROW: FuncallExit = emacs_funcall_exit_emacs_funcall_exit_throw;

// TODO: Error chain? (need to solve the issue that EmacsVal does not satisfy Send).
#[derive(Debug)]
pub enum NonLocalExit {
    Signal { symbol: EmacsVal, data: EmacsVal },
    Throw { tag: EmacsVal, value: EmacsVal },
}

pub type Result<T> = result::Result<T, NonLocalExit>;

// TODO: How about IntoEmacs (which may include EmacsVal itself)?
pub trait ToEmacs {
    fn to_emacs(&self, env: &Env) -> Result<EmacsVal>;
}

pub trait IntoEmacs {
    fn into_emacs(self, env: &Env) -> Result<EmacsVal>;
}

pub trait FromEmacs: Sized {
    fn from_emacs(env: *mut EmacsEnv, value: EmacsVal) -> Result<Self>;
}

pub struct Env {
    raw: *mut EmacsEnv
}

impl ToEmacs for i64 {
    fn to_emacs(&self, env: &Env) -> Result<EmacsVal> {
        let result = unsafe {
            let make_integer = (*env.raw).make_integer.unwrap();
            make_integer(env.raw, *self)
        };
        env.handle_exit(result)
    }
}

// TODO: Make this more elegant. Can't implement it for trait bound Into<Vec<u8>>, since that would
// complain about conflicting implementations for i64.
impl ToEmacs for str {
    fn to_emacs(&self, env: &Env) -> Result<EmacsVal> {
        // Rust string may fail to convert to CString. Raise non-local exit in that case.
        let cstring = env.to_cstring(self)?;
        let result = unsafe {
            let make_string = (*env.raw).make_string.unwrap();
            let ptr = cstring.as_ptr();
            make_string(env.raw, ptr, libc::strlen(ptr) as ptrdiff_t)
        };
        env.handle_exit(result)
    }
}

impl ToEmacs for String {
    fn to_emacs(&self, env: &Env) -> Result<EmacsVal> {
        self.as_str().to_emacs(env)
    }
}

impl ToEmacs for [Box<ToEmacs>] {
    fn to_emacs(&self, env: &Env) -> Result<EmacsVal> {
        let args = &mut env.to_emacs_args(self)?;
        env.list(args)
    }
}

// XXX: Doesn't work, possibly because of blanket implementations of Into. See
// https://github.com/rust-lang/rust/issues/30191 and
// https://github.com/rust-lang/rust/issues/19032.
//impl <T> ToEmacs for T where Vec<u8>: From<T> {
//    fn into_emacs(self, env: &Env) -> Result<EmacsVal> {
//        unimplemented!()
//    }
//}

impl IntoEmacs for i64 {
    fn into_emacs(self, env: &Env) -> Result<EmacsVal> {
        let result = unsafe {
            let make_integer = (*env.raw).make_integer.unwrap();
            make_integer(env.raw, self)
        };
        env.handle_exit(result)
    }
}

// XXX: Cannot do this. Box instances must contain sized types.
// Maybe related: https://github.com/rust-lang/rust/issues/27779.
//impl IntoEmacs for [Box<IntoEmacs>] {
//    fn into_emacs(self, env: &Env) -> Result<EmacsVal> {
//        unimplemented!()
//    }
//}

impl From<*mut EmacsEnv> for Env {
    fn from(raw: *mut EmacsEnv) -> Env {
        Env { raw }
    }
}

impl From<*mut EmacsRT> for Env {
    fn from(runtime: *mut EmacsRT) -> Env {
        let raw = unsafe {
            let get_env = (*runtime).get_environment.unwrap();
            get_env(runtime)
        };
        Env { raw }
    }
}

impl Env {
    pub fn raw(&self) -> *mut EmacsEnv {
        self.raw
    }

    fn non_local_exit_get(&self) -> (FuncallExit, EmacsVal, EmacsVal) {
        let symbol = Vec::<EmacsVal>::with_capacity(1).as_mut_ptr();
        let data = Vec::<EmacsVal>::with_capacity(1).as_mut_ptr();
        unsafe {
            let get = (*self.raw).non_local_exit_get.unwrap();
            let result = get(self.raw, symbol, data);
            (result, *symbol, *data)
        }
    }

    // TODO: Should we also clear the exit status, and leave it up to the Rust side whether to
    // continue propagating it.
    fn handle_exit<T>(&self, result: T) -> Result<T> {
        match self.non_local_exit_get() {
            (FUNCALL_EXIT_RETURN, ..) => Ok(result),
            (FUNCALL_EXIT_SIGNAL, symbol, data) => Err(NonLocalExit::Signal { symbol, data }),
            (FUNCALL_EXIT_THROW, tag, value) => Err(NonLocalExit::Throw { tag, value }),
            (status, ..) => panic!("Unexpected non local exit status {}", status),
        }
    }
    pub fn throw(&self, tag: EmacsVal, value: EmacsVal) -> Result<EmacsVal> {
        unsafe {
            let clear = (*self.raw).non_local_exit_clear.unwrap();
            let exit = (*self.raw).non_local_exit_throw.unwrap();
            // TODO: Add variants that don't clear the current exit status.
            clear(self.raw);
            exit(self.raw, tag, value);
        }
        Err(NonLocalExit::Throw { tag, value })
    }

    pub fn signal(&self, symbol: EmacsVal, data: EmacsVal) -> Result<EmacsVal> {
        unsafe {
            let clear = (*self.raw).non_local_exit_clear.unwrap();
            let exit = (*self.raw).non_local_exit_signal.unwrap();
            // TODO: Do we need variants that don't clear the current exit status?
            clear(self.raw);
            exit(self.raw, symbol, data);
        }
        Err(NonLocalExit::Signal { symbol, data })
    }

    pub fn error<T: ToEmacs>(&self, message: T) -> Result<EmacsVal> {
        let data = self.list(&mut [message.to_emacs(self)?])?;
        let symbol = self.intern("error")?;
        self.signal(symbol, data)
    }

    fn to_cstring(&self, s: &str) -> Result<CString> {
        CString::new(s).map_err(|_| {
            // TODO: Give more info, e.g. the string until null byte, or its position (if too long).
            self.error("Rust string with null byte cannot be converted to C string".to_owned())
                .unwrap_err()
        })
    }

    // TODO: Return a Symbol.
    pub fn intern(&self, name: &str) -> Result<EmacsVal> {
        let result = unsafe {
            let intern = (*self.raw).intern.unwrap();
            intern(self.raw, self.to_cstring(name)?.as_ptr())
        };
        self.handle_exit(result)
    }

    // TODO: Return a Symbol.
    pub fn type_of(&self, value: EmacsVal) -> Result<EmacsVal> {
        let result = unsafe {
            let type_of = (*self.raw).type_of.unwrap();
            type_of(self.raw, value)
        };
        self.handle_exit(result)
    }

    // TODO: Should there be variants of this that deal with mixtures of types?
    pub fn call(&self, name: &str, args: &mut [EmacsVal]) -> Result<EmacsVal> {
        let symbol = self.intern(name)?;
        let result = unsafe {
            let funcall = (*self.raw).funcall.unwrap();
            funcall(self.raw, symbol, args.len() as ptrdiff_t, args.as_mut_ptr())
        };
        self.handle_exit(result)
    }

//    pub fn call1(&self, name: &str, args: &mut [Box<ToEmacs>]) -> Result<EmacsVal> {
//        self.call(name,&mut self.to_emacs_args(args)?)
//    }

    fn to_emacs_args(&self, args: &[Box<ToEmacs>]) -> Result<Vec<EmacsVal>> {
        let mut e_args: Vec<EmacsVal> = Vec::with_capacity(args.len());
        for value in args.iter() {
            e_args.push(value.to_emacs(self)?);
        }
        Ok(e_args)
    }

    pub fn is_not_nil(&self, value: EmacsVal) -> Result<bool> {
        let result = unsafe {
            let is_not_nil = (*self.raw).is_not_nil.unwrap();
            is_not_nil(self.raw, value)
        };
        self.handle_exit(result)
    }

    pub fn eq(&self, a: EmacsVal, b: EmacsVal) -> Result<bool> {
        let result = unsafe {
            let eq = (*self.raw).eq.unwrap();
            eq(self.raw, a, b)
        };
        self.handle_exit(result)
    }

    pub fn list(&self, args: &mut [EmacsVal]) -> Result<EmacsVal> {
        self.call("list", args)
    }

    pub fn provide(&self, name: &str) -> Result<EmacsVal> {
        self.call("provide", &mut [self.intern(name)?])
    }

    pub fn fset(&self, name: &str, func: EmacsVal) -> Result<EmacsVal> {
        let i: i64 = 5;
        i.into_emacs(self)?;
        self.call("fset", &mut [
            self.intern(name)?, func
        ])
    }
}
