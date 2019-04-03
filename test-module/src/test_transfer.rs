// TODO: Add tests for Mutex and RwLock, and more tests for RefCell.
mod ref_cell {
    use emacs::{Result, IntoLisp};
    use std::cell::RefCell;

    /// Wrap the given integer in a RefCell.
    #[emacs::func(name = "ref-cell:make")]
    fn make(x: i64) -> Result<RefCell<i64>> {
        Ok(RefCell::new(x))
    }

    #[emacs::func(name = "ref-cell:mutate-twice")]
    fn mutate_twice(r: &RefCell<i64>) -> Result<()> {
        let mut x = r.try_borrow_mut()?;
        let mut y = r.try_borrow_mut()?;
        *x = 1;
        *y = 2;
        Ok(())
    }
}

mod hash_map {
    use std::cell::RefCell;
    use std::collections::HashMap;
    use emacs::{Result, Value, Env, IntoLisp, CallEnv};

    type Map = RefCell<HashMap<String, String>>;

    #[emacs::func(name = "hash-map:make")]
    fn make() -> Result<Map> {
        Ok(RefCell::new(HashMap::new()))
    }

    // XXX: Bad ergonomics.
    #[emacs::func(name = "hash-map:get")]
    fn get<'e>(env: &'e Env, map: &Map, key: String) -> Result<Value<'e>> {
        map.borrow().get(&key).into_lisp(env)
    }

    // XXX: Inefficient.
    #[emacs::func(name = "hash-map:get")]
    fn get0(map: &Map, key: String) -> Result<Option<String>> {
        Ok(map.borrow().get(&key).map(|s| s.to_owned()))
    }

    // TODO: Standardize on wrapping with RefCell, and generate code like below.
    #[allow(unused)]
    fn wrapper(env: &CallEnv) -> Result<Value<'_>> {
        type Map = HashMap<String, String>;

        // User code works with the inner type, instead of RefCell.
        fn inner(map: &Map, key: String) -> Result<Option<&String>> {
            Ok(map.get(&key))
        }

        // Generate additional borrowing code when user func uses reference type.
        let map = {
            let ref_cell: &RefCell<Map> = env.parse_arg(0)?;
            &*ref_cell.try_borrow()?
        };
        let key: String = env.parse_arg(1)?;
        let output = inner(map, key)?;
        output.into_lisp(env)
    }

    #[emacs::func(name = "hash-map:set")]
    fn set(map: &Map, key: String, value: String) -> Result<Option<String>> {
        Ok(map.borrow_mut().insert(key, value))
    }
}

mod vector {
    use emacs::{Result, Value, Env, IntoLisp};

    struct Vector {
        pub x: i64,
        pub y: i64,
    }

    custom_types! {
        Vector as "Vector";
    }

    #[emacs::func(name = "vector:swap-components")]
    fn swap_components(mut v: Value<'_>) -> Result<Value<'_>> {
        let vec: &mut Vector = unsafe { v.get_mut()? };
        vec.x = vec.x ^ vec.y;
        vec.y = vec.x ^ vec.y;
        vec.x = vec.x ^ vec.y;
        Ok(v)
    }

    #[emacs::func(name = "vector:make")]
    fn make(x: i64, y: i64) -> Result<Box<Vector>> {
        Ok(Box::new(Vector { x, y }))
    }

    #[emacs::func(name = "vector:to-list")]
    fn to_list<'e>(env: &'e Env, v: Value<'_>) -> Result<Value<'e>> {
        v.into_rust::<&Vector>()?;
        let v: &Vector = v.into_rust()?;
        let x = v.x.into_lisp(env)?;
        let y = v.y.into_lisp(env)?;
        env.list(&[x, y])
    }

    #[emacs::func(name = "vector:add")]
    fn add(a: &Vector, b: &Vector) -> Result<Box<Vector>> {
        let (x, y) = (b.x + a.x, b.y + a.y);
        Ok(Box::new(Vector { x, y }))
    }

    #[emacs::func(name = "vector:scale-mutably")]
    fn scale_mutably(times: i64, mut v: Value<'_>) -> Result<()> {
        let v = unsafe { v.get_mut::<Vector>()? };
        v.x *= times;
        v.y *= times;
        Ok(())
    }
}
