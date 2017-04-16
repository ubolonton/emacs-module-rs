use emacs_gen::{EmacsEnv, EmacsSubr, EmacsVal};
use std::os::raw;
use {call};
use std::ffi::CString;
use std::ptr;
use std::ops::Range;

pub unsafe extern "C" fn destruct<T>(arg: *mut raw::c_void) {
    let ptr = arg as *mut T;
    let lib = Box::from_raw(ptr);
    drop(lib);
    println!("Dropped value @ {:p}", ptr);
}

pub type ConvResult<T> = Result<T, ConvErr>;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ConvErr {
    CoreFnMissing(String),
    NulByteFound { pos: usize, bytes: Vec<u8> },
    Nullptr(String),
    StringLengthFetchFailed,
    StringCopyFailed,
}


#[no_mangle]
pub mod elisp2native {
    use emacs_gen::{EmacsEnv, EmacsVal};
    use hlapi::{ConvErr, ConvResult};
    use std::ptr;

    pub fn pointer<T>(env: *mut EmacsEnv, args: *mut EmacsVal, index: usize)
                      -> ConvResult<*mut T> {
        if args.is_null() { return Err(ConvErr::Nullptr(String::from("args"))) }
        // TODO: verify that `index` is within bounds
        unsafe { match (*env).get_user_ptr {
            None => Err(ConvErr::CoreFnMissing(String::from("get_user_ptr"))),
            Some(get_user_ptr) => {
                let arg = *args.offset(index as isize);
                Ok(get_user_ptr(env, arg) as *mut T)
            },
        }}
    }

    pub fn string(env: *mut EmacsEnv, args: *mut EmacsVal, index: usize)
                  -> ConvResult<Vec<i8>> {
        unsafe {
            let mut length: isize = 0;
            let copy_string_contents = (*env).copy_string_contents.ok_or_else(
                || ConvErr::CoreFnMissing(String::from("copy_string_contents"))
            )?;
            let ok = copy_string_contents(env,
                                          *args.offset(index as isize),
                                          ptr::null_mut(),
                                          &mut length as *mut isize);
            if !ok { return Err(ConvErr::StringLengthFetchFailed) }

            let mut string = vec![0; length as usize];
            let ok = copy_string_contents(env,
                                          *args.offset(index as isize),
                                          string.as_mut_ptr(),
                                          &mut length as *mut isize);
            if !ok { return Err(ConvErr::StringCopyFailed) }
            Ok(string)
        }
    }

    pub fn integer(env: *mut EmacsEnv, args: *mut EmacsVal, index: usize)
                   -> ConvResult<i64> {
        if args.is_null() { return Err(ConvErr::Nullptr(String::from("args"))) }
        // TODO: verify that `index` is within bounds
        unsafe {
            let arg = *args.offset(index as isize);
            let extract_int = (*env).extract_integer.ok_or_else(
                || ConvErr::CoreFnMissing(String::from("extract_integer"))
            )?;
            Ok(extract_int(env, arg))
        }
    }

}

pub mod native2elisp {
    use emacs_gen::{EmacsEnv, EmacsSubr, EmacsVal};
    use hlapi::{ConvErr, ConvResult};
    use libc;
    use std::ffi::CString;
    use std::os::raw;
    use {call};

    pub fn integer(env: *mut EmacsEnv, num: i64) -> ConvResult<EmacsVal> {
        unsafe {
            let make_integer = (*env).make_integer.ok_or_else(
                || ConvErr::CoreFnMissing(String::from("make_string")))?;
            Ok(make_integer(env, num))
        }
    }

    /// Convert a Rust String/&str into an Elisp string.
    pub fn string<S>(env: *mut EmacsEnv, string: S)
                     -> ConvResult<EmacsVal>  where S: Into<Vec<u8>> {
        unsafe {
            let string: Vec<u8> = string.into();
            let cstring = CString::new(string)
                .map_err(|nul_err| ConvErr::NulByteFound {
                    pos: nul_err.nul_position(),
                    bytes: nul_err.into_vec(),
                })?;
            let c_string: *const libc::c_char = cstring.as_ptr();
            let strlen: usize = libc::strlen(c_string);
            let make_string = (*env).make_string
                .ok_or(ConvErr::CoreFnMissing(String::from("make_string")))?;
            Ok(make_string(env, c_string, strlen as isize))
        }
    }

    /// Intern a new Elisp symbol.
    pub fn symbol(env: *mut EmacsEnv, name: &str) -> ConvResult<EmacsVal> {
        Ok(call(env, "intern", &mut [string(env, name)?]))
    }

    pub fn function(env: *mut EmacsEnv,
                    min_arity: isize,
                    max_arity: isize,
                    function: Option<EmacsSubr>,
                    documentation: *const libc::c_char,
                    data: *mut raw::c_void) -> EmacsVal {
        unsafe {
            let make_fn = (*env).make_function.unwrap();
            make_fn(env, min_arity, max_arity, function, documentation, data)
        }
    }
}


/// A μDSL to cut away boilerplate when defining Emacs subrs,
/// which are of course defined in Rust rather than C here.
/// One thing enforced at compile time is that some semblance
/// of a legal Elisp value is returned.
#[macro_export]
macro_rules! emacs_subrs {
    ($($name:ident($env:ident, $nargs:ident, $args:ident, $data:ident, $tag:ident)
       $body:expr;)*) => {
        $(
            #[allow(non_snake_case, unused_variables)]
            unsafe extern "C" fn $name($env: *mut EmacsEnv,
                                       $nargs: libc::ptrdiff_t,
                                       $args: *mut EmacsVal,
                                       $data: *mut raw::c_void)
                                       -> EmacsVal {
                let $tag = format!("[ZMQ/{}]", stringify!($name));
                $body
            }
        )*
    };
}



#[macro_export]
macro_rules! message {
    ($env:expr, $fmt:expr $(, $args:expr)*) => {{
        use $crate::hlapi;
        hlapi::message($env, format!($fmt $(, $args)*))
    }};
}

/// Log a message to the *Messages* buffer.
pub fn message<S>(env: *mut EmacsEnv, text: S)
                  -> ConvResult<EmacsVal>  where S: Into<String> {
    let string = native2elisp::string(env, text.into())?;
    Ok(call(env, "message", &mut [string]))
}

/// Basic Elisp equality check.
pub fn eq(env: *mut EmacsEnv, left: EmacsVal, right: EmacsVal) -> bool {
    unsafe {
        let eq = (*env).eq.unwrap();
        eq(env, left, right)
    }
}


/// Register an Emacs `subr`, so that it can be accessed from Elisp.
pub fn register(env: *mut EmacsEnv,
                elisp_sym: &str,
                native_sym: EmacsSubr,
                nargs_range: Range<usize>,
                docstring: &str,
                /* user_ptr: *mut libc::c_void*/) {
    let doc = CString::new(docstring).unwrap().as_ptr();
    let func = native2elisp::function(env,
                                      nargs_range.start as isize,
                                      nargs_range.end as isize + 1,
                                      Some(native_sym),
                                      doc,
                                      ptr::null_mut(/* user_ptr */));
    let elisp_symbol = match native2elisp::symbol(env, elisp_sym) {
        Ok(string) => string,
        Err(conv_err) => {
            message!(env, "Error: {:?}", conv_err);
            return;
        },
    };
    call(env, "fset", &mut [elisp_symbol, func]);
    message!(env, "Registered function {}", elisp_sym);
}
