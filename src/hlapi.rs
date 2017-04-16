use emacs_gen::{EmacsEnv, EmacsVal};
use std::os::raw;

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
    use emacs_gen::{EmacsEnv, EmacsVal};
    use libc;
    use std::ffi::CString;
    use hlapi::{ConvErr, ConvResult};
    use call;

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
