use emacs::{Env, CallEnv, Value, IntoLisp, Result};

use super::MODULE_PREFIX;

fn gc(env: &CallEnv) -> Result<Value> {
    env.call("garbage-collect", &[])
}

fn print<'e>(env: &'e CallEnv, v: Value) -> Result<Value<'e>> {
    env.call("print", &[v])
}

fn create_collect_use<'e, CF, UF>(
    env: &'e CallEnv,
    count: usize,
    creating: CF,
    using: UF,
) -> Result<Value>
    where CF: Fn() -> Result<Value<'e>>,
          UF: Fn(&'e CallEnv, Value) -> Result<Value<'e>>,
{
    let mut v = Vec::with_capacity(count);
    for _ in 0..count {
        let x = creating()?;
        v.push(x);
    }

    gc(env)?;
    let l = env.list(&v)?;
    // Seems like the last value is not GC'ed, so loop backward.
    for i in (0..count).rev() {
        println!("using {}", i);
        using(env, v[i])?;
    }
    Ok(l)
}

// macOS: Segmentation fault
fn gc_after_new_string(env: &CallEnv) -> Result<Value> {
    creating_gc_listing_using(env, 2, || {
        "0".into_lisp(env)
    }, print)
}

// macOS: Segmentation fault
fn gc_after_uninterning(env: &CallEnv) -> Result<Value> {
    creating_gc_listing_using(env, 2, || {
        let x = env.intern("xyz")?;
        env.call("unintern", &[x])?;
        Ok(x)
    }, print)
}

// macOS: Abort trap (since the violation happens in Rust)
fn gc_after_retrieving(env: &CallEnv) -> Result<Value> {
    creating_gc_listing_using(env, 2, || {
        // XXX: These come from `test_transfer` module.
        env.call(&format!("{}hash-map:make", *MODULE_PREFIX), &[])
    }, |env, v| {
        print(env, v)?; // Used: #<user-ptr ptr=... finalizer=...>. Free: #<misc free cell>.
        env.call(&format!("{}hash-map:set", *MODULE_PREFIX), &[
            v,
            "x".into_lisp(env)?,
            "y".into_lisp(env)?,
        ])
    })
}

pub fn init(env: &Env) -> Result<()> {
    emacs_export_functions! {
        env, *MODULE_PREFIX, {
            "gc-after-new-string" => (gc_after_new_string, 0..0),
            "gc-after-uninterning" => (gc_after_uninterning, 0..0),
            "gc-after-retrieving" => (gc_after_retrieving, 0..0),
        }
    }

    Ok(())
}
