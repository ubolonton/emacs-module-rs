#![allow(non_upper_case_globals)]

global_refs! {common(init_to_symbol) =>
    nil t
    rust_error => crate::error::ERROR
    rust_panic => crate::error::PANIC
    rust_wrong_type_user_ptr => crate::error::WRONG_TYPE_USER_PTR
}
