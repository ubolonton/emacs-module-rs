use std::result;
use std::error;
use std::io;
use std::ffi::NulError;
use emacs_gen::*;
use new::{Env, ToEmacs};

/// We assume that the C code in Emacs really treats it as an enum and doesn't return an undeclared
/// value, but we still need to safeguard against possible compatibility issue (Emacs may add more
/// statuses in the future). FIX: Use an enum, and check for compatibility on load. Possible or not?
pub type FuncallExit = emacs_funcall_exit;

const RETURN: FuncallExit = emacs_funcall_exit_emacs_funcall_exit_return;
const SIGNAL: FuncallExit = emacs_funcall_exit_emacs_funcall_exit_signal;
const THROW: FuncallExit = emacs_funcall_exit_emacs_funcall_exit_throw;

#[derive(Debug)]
pub struct Error {
    pub(crate) kind: ErrorKind
}

// TODO: Use error-chain? (need to solve the issue that EmacsVal does not satisfy Send).
#[derive(Debug)]
pub enum ErrorKind {
    Signal { symbol: EmacsVal, data: EmacsVal },
    Throw { tag: EmacsVal, value: EmacsVal },
    IO { error: io::Error },
    Other { error: Box<error::Error+Send> },
}

pub type Result<T> = result::Result<T, Error>;

impl Error {
    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    pub fn signal(symbol: EmacsVal, data: EmacsVal) -> Self {
        Self { kind: ErrorKind::Signal { symbol, data } }
    }

    pub fn throw(tag: EmacsVal, value: EmacsVal) -> Self {
        Self { kind: ErrorKind::Throw { tag, value } }
    }
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Self {
        Self { kind: ErrorKind::IO { error } }
    }
}

// TODO: Better reporting.
impl From<NulError> for Error {
    fn from(error: NulError) -> Self {
        Self { kind: ErrorKind::Other { error: Box::new(error) }}
    }
}

// TODO: Use these only in the internal functions that call Emacs.
pub(crate) trait HandleExit {
    fn handle_exit<T>(&self, result: T) -> Result<T>;
}

fn non_local_exit_get(env: &Env) -> (FuncallExit, EmacsVal, EmacsVal) {
    let symbol = Vec::<EmacsVal>::with_capacity(1).as_mut_ptr();
    let data = Vec::<EmacsVal>::with_capacity(1).as_mut_ptr();
    unsafe {
        let get = (*env.raw).non_local_exit_get.unwrap();
        let result = get(env.raw, symbol, data);
        (result, *symbol, *data)
    }
}

fn non_local_exit_clear(env: &Env) -> Result<()> {
    unsafe {
        let clear = (*env.raw).non_local_exit_clear.unwrap();
        clear(env.raw)
    }
    Ok(())
}

impl HandleExit for Env {
    fn handle_exit<T>(&self, result: T) -> Result<T> {
//        println!("handling exit");
        match non_local_exit_get(self) {
            (RETURN, ..) => Ok(result),
            (SIGNAL, symbol, data) => {
                println!("signaled");
                non_local_exit_clear(self)?;
                Err(Error::signal(symbol, data))
            },
            (THROW, tag, value) => {
                println!("thrown");
                non_local_exit_clear(self)?;
                Err(Error::throw(tag, value))
            },
            // TODO: Don't panic here, use a custom error.
            (status, ..) => panic!("Unexpected non local exit status {}", status),
        }
    }
}

// TODO: Use these only in the wrapper funcs that give the error back to Emacs. One problem is,
// wrappers are written (by macros) by user code, which shouldn't have access to these.
pub trait TriggerExit {
    fn maybe_exit(&self, result: Result<EmacsVal>) -> EmacsVal;
}

fn throw(env: &Env, tag: EmacsVal, value: EmacsVal) -> EmacsVal {
    unsafe {
        let exit = (*env.raw).non_local_exit_throw.unwrap();
        exit(env.raw, tag, value);
    }
    tag
}

fn signal(env: &Env, symbol: EmacsVal, data: EmacsVal) -> EmacsVal {
    unsafe {
        // Note: If this function is missing, there is no way to report error to Emacs. The only
        // sensible thing to do is crashing. Therefore unwrap() is used. Other places should use
        // "function-missing" error.
        let exit = (*env.raw).non_local_exit_signal.unwrap();
        exit(env.raw, symbol, data);
    }
    symbol
}

// XXX
fn error(env: &Env, message: &str) -> Result<EmacsVal> {
    let data = env.list(&mut [message.to_emacs(env)?])?;
    let symbol = env.intern("error")?;
    Ok(signal(env, symbol, data))
}

impl TriggerExit for Env {
    /// This is intended to be used at the Rust->Emacs boundary, by the internal macros/functions.
    /// Module code should use [`Error::throw`] and [`Error::signal`] instead.
    fn maybe_exit(&self, result: Result<EmacsVal>) -> EmacsVal {
        match result {
            Ok(v) => v,
            Err(e) => {
                match e.kind {
                    ErrorKind::Signal { symbol, data } => signal(self, symbol, data),
                    ErrorKind::Throw { tag, value } => throw(self, tag, value),
                    // XXX: Custom error instead of panicking.
                    _ => error(self, "Hmm").unwrap(),
                }
            }
        }
    }
}
