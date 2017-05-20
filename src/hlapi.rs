use emacs_gen::{EmacsEnv, EmacsSubr, EmacsVal};
use std::os::raw;
use {call};
use std::ffi::{CString, FromBytesWithNulError, IntoStringError, NulError};
use std::io;
use std::ops::Range;
use std::ptr;
use std::string::FromUtf8Error;
use std::str::Utf8Error;

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
    Nullptr(String),
    FailedToFetchLength,
    FailedToCopy,
    InvalidArgCount(usize),
    Other(String),
    Interrupted,
    DoNothing,
    WrongEmacsValueType { expected: String, got: Option<EmacsVal> },

    IoNotFound(Option<String>),
    IoPermissionDenied(Option<String>),
    IoConnectionRefused(Option<String>),
    IoConnectionReset(Option<String>),
    IoConnectionAborted(Option<String>),
    IoNotConnected(Option<String>),
    IoAddrInUse(Option<String>),
    IoAddrNotAvailable(Option<String>),
    IoBrokenPipe(Option<String>),
    IoAlreadyExists(Option<String>),
    IoWouldBlock(Option<String>),
    IoInvalidInput(Option<String>),
    IoInvalidData(Option<String>),
    IoTimedOut(Option<String>),
    IoWriteZero(Option<String>),
    IoInterrupted(Option<String>),
    IoOther(Option<String>),
    IoUnexpectedEof(Option<String>),

    NulByteFound { pos: usize, bytes: Vec<u8> },

    FromUtf8Error { valid_up_to: usize,  bytes: Vec<u8> },

    Utf8Error { valid_up_to: usize },

    FromBytesWithNul,
}

impl From<io::Error> for ConvErr {
    fn from(ioe: io::Error) -> ConvErr {
        use std::io::ErrorKind;
        use std::error::Error;
        let cause = ioe.cause().map(|err: &Error| format!("{}", err));
        match ioe.kind() {
            ErrorKind::NotFound => ConvErr::IoNotFound(cause),
            ErrorKind::PermissionDenied => ConvErr::IoPermissionDenied(cause),
            ErrorKind::ConnectionRefused => ConvErr::IoConnectionRefused(cause),
            ErrorKind::ConnectionReset => ConvErr::IoConnectionReset(cause),
            ErrorKind::ConnectionAborted => ConvErr::IoConnectionAborted(cause),
            ErrorKind::NotConnected => ConvErr::IoNotConnected(cause),
            ErrorKind::AddrInUse => ConvErr::IoAddrInUse(cause),
            ErrorKind::AddrNotAvailable => ConvErr::IoAddrNotAvailable(cause),
            ErrorKind::BrokenPipe => ConvErr::IoBrokenPipe(cause),
            ErrorKind::AlreadyExists => ConvErr::IoAlreadyExists(cause),
            ErrorKind::WouldBlock => ConvErr::IoWouldBlock(cause),
            ErrorKind::InvalidInput => ConvErr::IoInvalidInput(cause),
            ErrorKind::InvalidData => ConvErr::IoInvalidData(cause),
            ErrorKind::TimedOut => ConvErr::IoTimedOut(cause),
            ErrorKind::WriteZero => ConvErr::IoWriteZero(cause),
            ErrorKind::Interrupted => ConvErr::IoInterrupted(cause),
            ErrorKind::Other => ConvErr::IoOther(cause),
            ErrorKind::UnexpectedEof => ConvErr::IoUnexpectedEof(cause),
            _ => unimplemented!(),
        }
    }
}


impl From<NulError> for ConvErr {
    fn from(nerr: NulError) -> ConvErr {
        ConvErr::NulByteFound {
            pos: nerr.nul_position(),
            bytes: nerr.into_vec(),
        }
    }
}

impl From<FromUtf8Error> for ConvErr {
    fn from(fue: FromUtf8Error) -> ConvErr {
        ConvErr::FromUtf8Error {
            valid_up_to: fue.utf8_error().valid_up_to(),
            bytes: fue.into_bytes()
        }
    }
}

impl From<Utf8Error> for ConvErr {
    fn from(ue: Utf8Error) -> ConvErr {
        ConvErr::Utf8Error { valid_up_to: ue.valid_up_to() }
    }
}

impl From<FromBytesWithNulError> for ConvErr {
    fn from(_: FromBytesWithNulError) -> ConvErr {
        ConvErr::FromBytesWithNul
    }
}

impl From<IntoStringError> for ConvErr {
    fn from(ise: IntoStringError) -> ConvErr {
        ConvErr::from(ise.utf8_error())
    }
}


pub mod elisp2native {
    use emacs_gen::{EmacsEnv, EmacsVal};
    use hlapi::{ConvErr, ConvResult};
    use std::ffi::{CString};
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

    pub fn mut_ref<'t, T>(env: *mut EmacsEnv, args: *mut EmacsVal, index: usize)
                          -> ConvResult<&'t mut T> {
        pointer(env, args, index).map(|raw: *mut T| unsafe { &mut *raw })
    }

    pub fn string(env: *mut EmacsEnv, val: EmacsVal) -> ConvResult<String> {
        let bytes: Vec<u8> = self::string_bytes(env, val)?;
        Ok(String::from_utf8(bytes)?)
    }

    pub fn cstring(env: *mut EmacsEnv, val: EmacsVal) -> ConvResult<CString> {
        let mut bytes: Vec<u8> = self::string_bytes(env, val)?;
        let mut len = bytes.len();
        while len > 0 && bytes[len - 1] == 0 {
            bytes.pop(); // strip trailing 0-bytes
            len = bytes.len();
        }
        Ok(CString::new(bytes)?)
    }

    pub fn string_bytes(env: *mut EmacsEnv, val: EmacsVal)
                        -> ConvResult<Vec<u8>> {
        let mut len: isize = 0;
        unsafe {
            // Fetch Elisp path string length
            let copy_string_contents = (*env).copy_string_contents.ok_or_else(
                || ConvErr::CoreFnMissing(String::from("copy_string_contents"))
            )?;
            let ok = copy_string_contents(env, val, ptr::null_mut(), &mut len);
            if !ok { return Err(ConvErr::FailedToFetchLength); }

            // Copy the Elisp path string to a Rust Vec, based on its length
            let mut bytes = vec![0u8; len as usize];
            let bytes_ptr = bytes.as_mut_ptr() as *mut i8;
            let ok = copy_string_contents(env, val, bytes_ptr, &mut len);
            if !ok { return Err(ConvErr::FailedToCopy); }
            Ok(bytes)
        }
    }

    pub fn integer(env: *mut EmacsEnv, args: *mut EmacsVal, index: usize)
                   -> ConvResult<i64> {
        if args.is_null() { return Err(ConvErr::Nullptr(String::from("args"))) }
        // TODO: verify that `index` is within bounds
        unsafe {
            int_value(env, *args.offset(index as isize))
        }
    }

    pub fn int_value(env: *mut EmacsEnv, val: EmacsVal) -> ConvResult<i64> {
        if val.is_null() { return Err(ConvErr::Nullptr(String::from("val"))) }
        unsafe {
            let extract_int = (*env).extract_integer.ok_or_else(
                || ConvErr::CoreFnMissing(String::from("extract_integer"))
            )?;
            Ok(extract_int(env, val))
        }
    }


    pub fn list(env: *mut EmacsEnv, arg: EmacsVal) -> ConvResult<Vec<EmacsVal>> {
        let nil: EmacsVal = ::native2elisp::symbol(env, "nil")?;
        let is_list: EmacsVal = ::call(env, "listp", &mut [arg]);
        if ::eq(env, is_list, nil)? {
            return Err(ConvErr::WrongEmacsValueType {
                expected: String::from("list"),
                got: Some(arg)
            });
        }
        let length: EmacsVal = ::call(env, "length", &mut [arg]);
        let length: i64 = int_value(env, length)?;
        let mut list: Vec<EmacsVal> = vec![];
        for i in 0 .. length {
            let index: EmacsVal = ::native2elisp::integer(env, i)?;
            let element: EmacsVal = ::call(env, "nth", &mut [index, arg]);
            list.push(element);
        }
        Ok(list)
    }

}

pub mod native2elisp {
    use emacs_gen::{Dtor, EmacsEnv, EmacsSubr, EmacsVal};
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
                    data: *mut raw::c_void)
                    -> ConvResult<EmacsVal> {
        unsafe {
            let make_fn = (*env).make_function.ok_or_else(
                || ConvErr::CoreFnMissing(String::from("make_function"))
            )?;
            Ok(make_fn(env, min_arity, max_arity, function, documentation, data))
        }
    }

    /// Transform a `Box<T>` into a `*mut EmacsVal`.
    pub fn boxed<T>(env: *mut EmacsEnv, value: T, dtor: Dtor)
                    -> ConvResult<EmacsVal> {
        let ptr = Box::into_raw(Box::new(value)) as *mut raw::c_void;
        println!("Transferred Box<T> @ {:p} to Elisp", ptr);
        unsafe {
            let make_user_ptr = (*env).make_user_ptr.ok_or_else(
                || ConvErr::CoreFnMissing(String::from("make_user_ptr"))
            )?;
            Ok(make_user_ptr(env, Some(dtor), ptr))
        }
    }

    pub fn string_list<S>(env: *mut EmacsEnv, strings: &[S])
                          -> ConvResult<EmacsVal>   where S: AsRef<str> {
        let mut list: EmacsVal = ::call(env, "list", &mut []);
        for entry in strings.iter().rev() {
            list = ::call(env, "cons", &mut [
                ::native2elisp::string(env, entry.as_ref())?,
                list
            ]);
        }
        Ok(list)
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
                // NOTE: The inner `fun` fn provides type checking for Emacs
                // subrs -- especially their output -- while also allowing each
                // subr to just use `?` for error handling. This is much nicer
                // than any alternative would be. It does mean that those errors
                // need to be dealt with here. For now it calls `expect()` which
                // still means a panic. However at least the cause should be
                // clear because of a useful backtrace, as well as proper error
                // handling in the subrs themselves. This in turn aids debugging.
                //
                // Inlining the inner fn means there's no runtime penalty at the
                // cost of slightly higher compile times.
                #[inline(always)]
                unsafe fn fun($env: *mut EmacsEnv,
                              $nargs: libc::ptrdiff_t,
                              $args: *mut EmacsVal,
                              $data: *mut raw::c_void,
                              $tag: &str) -> ConvResult<EmacsVal> { $body }

                let $tag = format!("[{}]", stringify!($name));
                match fun($env, $nargs, $args, $data, &$tag) {
                    Ok(value) => value,
                    Err(conv_err) => {
                        let msg = format!("{} ConvErr::{:?}", $tag, conv_err);
                        $crate::hlapi::native2elisp::string($env, msg)
                        // TODO: implement sans panic using the ?-operator
                            .expect("Error string creation failed")
                    }
                }
            }
        )*
    };
}

#[macro_export]
macro_rules! init_module {
    (($env:ident) $body:expr ) => {
        #[no_mangle]
        pub extern "C" fn emacs_module_init(runtime: *mut EmacsRT)
                                            -> libc::c_int {{
            #[inline(always)]
            fn fun($env: *mut EmacsEnv) -> ConvResult<EmacsVal> { $body }

            let $env = emacs::get_environment(runtime);
            let result: ConvResult<EmacsVal> = fun($env);
            result.expect("$body should yield an Ok value");
            0
        }}
    };
}



#[macro_export]
macro_rules! message {
    ($env:expr, $fmt:expr $(, $args:expr)*) => {{
        $crate::hlapi::message($env, format!($fmt $(, $args)*))
    }};
}

/// Log a message to the *Messages* buffer.
pub fn message<S>(env: *mut EmacsEnv, text: S)
                  -> ConvResult<EmacsVal>  where S: Into<String> {
    let string = native2elisp::string(env, text.into())?;
    Ok(call(env, "message", &mut [string]))
}

/// Basic Elisp equality check.
pub fn eq(env: *mut EmacsEnv, left: EmacsVal, right: EmacsVal)
          -> ConvResult<bool> {
    unsafe {
        let eq = (*env).eq.ok_or_else(
            || ConvErr::CoreFnMissing(String::from("eq"))
        )?;
        Ok(eq(env, left, right))
    }
}


/// Register an Emacs `subr`, so that it can be accessed from Elisp.
pub fn register(env: *mut EmacsEnv,
                elisp_sym: &str,
                native_sym: EmacsSubr,
                nargs_range: Range<usize>,
                docstring: &str,
                /* user_ptr: *mut libc::c_void*/)
                -> ConvResult<EmacsVal> {
    let doc = CString::new(docstring)?.as_ptr();
    let func = native2elisp::function(env,
                                      nargs_range.start as isize,
                                      nargs_range.end as isize + 1,
                                      Some(native_sym),
                                      doc,
                                      ptr::null_mut(/* user_ptr */))?;
    let elisp_symbol = native2elisp::symbol(env, elisp_sym)?;
    call(env, "fset", &mut [elisp_symbol, func]);
    message!(env, "Registered function {}", elisp_sym)?;
    native2elisp::symbol(env, "t")
}
