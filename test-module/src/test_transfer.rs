use std::cell::RefCell;

use emacs::{Env, CallEnv, Value, IntoLisp, Result, ResultExt};

use super::MODULE_PREFIX;

// TODO: Add tests for Mutex and RwLock, and more tests for RefCell.
fn expose_ref_cell(env: &Env) -> Result<()> {
    fn make(env: &CallEnv) -> Result<RefCell<i64>> {
        let x: i64 = env.parse_arg(0)?;
        Ok(RefCell::new(x))
    }

    fn mutate_twice(env: &CallEnv) -> Result<()> {
        let r = env.get_arg(0);
        let r: &RefCell<i64> = r.into_rust()?;
        let mut x = r.try_borrow_mut().context("test-module")?;
        let mut y = r.try_borrow_mut().context("test-module")?;
        *x = 1;
        *y = 2;
        Ok(())
    }

    emacs_export_functions! {
        env, format!("{}ref-cell:", *MODULE_PREFIX), {
            "make"         => (make, 1..1, "Wrap the given integer in a RefCell."),
            "mutate-twice" => (mutate_twice, 1..1, "This should fail at run time due to double mut borrows."),
        }
    }

    Ok(())
}

fn expose_hash_map(env: &Env) -> Result<()> {
    use std::cell::RefCell;
    use std::collections::HashMap;

    type Map = RefCell<HashMap<String, String>>;

    fn make(_: &CallEnv) -> Result<Map> {
        Ok(RefCell::new(HashMap::new()))
    }

    fn get(env: &CallEnv) -> Result<Value> {
        let map: &Map = env.parse_arg(0)?;
        let key: String = env.parse_arg(1)?;
        map.borrow().get(&key).into_lisp(env)
    }

    fn set(env: &CallEnv) -> Result<Option<String>> {
        let map: &Map = env.parse_arg(0)?;
        let key: String = env.parse_arg(1)?;
        let value: String = env.parse_arg(2)?;
        Ok(map.borrow_mut().insert(key, value))
    }

    emacs_export_functions! {
        env, format!("{}hash-map:", *MODULE_PREFIX), {
            "make"    => (make, 0..0),
            "get"     => (get, 2..2),
            "set"     => (set, 3..3),
        }
    }

    Ok(())
}

fn expose_custom_vector(env: &Env) -> Result<()> {
    struct Vector {
        pub x: i64,
        pub y: i64,
    }

    custom_types! {
        Vector as "Vector";
    }

    fn swap_components(env: &CallEnv) -> Result<Value> {
        let mut v = env.get_arg(0);
        {
            let vec: &mut Vector = unsafe { v.get_mut()? };
            vec.x = vec.x ^ vec.y;
            vec.y = vec.x ^ vec.y;
            vec.x = vec.x ^ vec.y;
        }
        Ok(v)
    }

    emacs_export_functions! {
        env, format!("{}vector:", *MODULE_PREFIX), {
            "swap-components" => (swap_components, 1..1)
        }
    }

    defuns! {
        env, format!("{}vector:", *MODULE_PREFIX);

        "make", "", (env, x, y) {
            let x: i64 = x.into_rust()?;
            let y: i64 = y.into_rust()?;
            let b = Box::new(Vector { x, y });
            b.into_lisp(env)
        }

        "to-list", "", (env, v) {
            v.into_rust::<&Vector>()?;
            let v: &Vector = v.into_rust()?;
            let x = v.x.into_lisp(env)?;
            let y = v.y.into_lisp(env)?;
            env.list(&[x, y])
        }

        "add", "", (env, a, b) {
            let a: &Vector = a.into_rust()?;
            let b: &Vector = b.into_rust()?;
            let (x, y) = (b.x + a.x, b.y + a.y);
            Box::new(Vector { x, y }).into_lisp(env)
        }

        "scale-mutably", "", (env, times, v) {
            let times: i64 = times.into_rust()?;
            {
                let mut v = v;
                let v = unsafe { v.get_mut::<Vector>()? };
                v.x *= times;
                v.y *= times;
            }
            env.intern("nil")
        }
    }

    Ok(())
}

pub fn init(env: &Env) -> Result<()> {
    expose_custom_vector(env)?;
    expose_ref_cell(env)?;
    expose_hash_map(env)?;
    Ok(())
}
