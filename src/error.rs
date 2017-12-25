use std::result;
use std::error;
use std::io;
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

impl HandleExit for Env {
    fn handle_exit<T>(&self, result: T) -> Result<T> {
        match non_local_exit_get(self) {
            (RETURN, ..) => Ok(result),
            (SIGNAL, symbol, data) => Err(Error::signal(symbol, data)),
            (THROW, tag, value) => Err(Error::throw(tag, value)),
            // TODO: Don't panic here, use a custom error.
            (status, ..) => panic!("Unexpected non local exit status {}", status),
        }
    }
}

// TODO: Use these only in the wrapper funcs that give the error back to Emacs.
pub(crate) trait TriggerExit {
    fn throw(&self, tag: EmacsVal, value: EmacsVal) -> Result<EmacsVal>;
    fn signal(&self, symbol: EmacsVal, data: EmacsVal) -> Result<EmacsVal> ;
    fn error<T: ToEmacs>(&self, message: T) -> Result<EmacsVal>;
}

impl TriggerExit for Env {
    fn throw(&self, tag: EmacsVal, value: EmacsVal) -> Result<EmacsVal> {
        unsafe {
            let clear = (*self.raw).non_local_exit_clear.unwrap();
            let exit = (*self.raw).non_local_exit_throw.unwrap();
            clear(self.raw);
            exit(self.raw, tag, value);
        }
        Err(Error::throw(tag, value))
    }

    fn signal(&self, symbol: EmacsVal, data: EmacsVal) -> Result<EmacsVal> {
        unsafe {
            let clear = (*self.raw).non_local_exit_clear.unwrap();
            let exit = (*self.raw).non_local_exit_signal.unwrap();
            clear(self.raw);
            exit(self.raw, symbol, data);
        }
        Err(Error::signal(symbol, data))
    }

    fn error<T: ToEmacs>(&self, message: T) -> Result<EmacsVal> {
        let data = self.list(&mut [message.to_emacs(self)?])?;
        let symbol = self.intern("error")?;
        self.signal(symbol, data)
    }
}
