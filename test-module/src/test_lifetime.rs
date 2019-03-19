use emacs::{Env, CallEnv, Value, IntoLisp, Result};
use emacs::ErrorKind::{self, Signal};

use super::MODULE_PREFIX;

fn gc(env: &CallEnv) -> Result<Value<'_>> {
    env.call("garbage-collect", &[])
}

fn print<'e>(env: &'e CallEnv, v: Value<'_>) -> Result<Value<'e>> {
    env.call("print", &[v])
}

fn create_collect_use<'e, CF, UF>(
    env: &'e CallEnv,
    count: usize,
    creating: CF,
    using: UF,
) -> Result<Value<'_>>
    where CF: Fn() -> Result<Value<'e>>,
          UF: Fn(&'e CallEnv, Value<'_>) -> Result<Value<'e>>,
{
    // - It's interesting that it wouldn't crash if the loop is unrolled.
    // - Even more interesting is it'd crash when manual malloc+free is used in raw C
    // (see C: https://github.com/ubolonton/emacs-module-crash/blob/59f60c2/main.c#L51).
    //
    // It seems like, when loops/closures are used, Rust stack/heap "fool" Emacs's conservative GC
    // into seeing the references. That's actually the better behavior IMO, but Emacs for some
    // reasons decided otherwise (see https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31238 and
    // https://github.com/emacs-mirror/emacs/commit/3eb93c0#diff-284aeec59d62ef255c4a4e773900924bL954).
    let mut v = Vec::with_capacity(count);
    for _ in 0..count {
        let x = creating()?;
        v.push(x);
    }

    gc(env)?;

    // Seems like the last value is not GC'ed, so loop backward (mainly to help with debugging).
    for i in (0..count).rev() {
        println!("using {}", i);
        using(env, v[i])?;
    }

    Ok(v[0])
}

// Before fixing:
// - macOS: Segmentation fault
// - Linux: Segmentation fault
fn gc_after_new_string(env: &CallEnv) -> Result<Value<'_>> {
    create_collect_use(env, 2, || {
        "0".into_lisp(env)
    }, print)
}

// Before fixing:
// - macOS: Segmentation fault
// - Linux: Segmentation fault
fn gc_after_uninterning(env: &CallEnv) -> Result<Value<'_>> {
    // Wouldn't fail if count is 1 or 2.
    create_collect_use(env, 3, || {
        let x = env.intern("xyz")?;
        env.call("unintern", &[x])?;
        Ok(x)
    }, print)
}

// Before fixing:
// - macOS: Abort trap (since the violation happens in Rust)
// - Linux: wrong-type-argument (maybe the runtime is a bit different in Linux?)
fn gc_after_retrieving(env: &CallEnv) -> Result<Value<'_>> {
    create_collect_use(env, 2, || {
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

fn gc_after_catching_1(env: &CallEnv) -> Result<Value<'_>> {
    let f = env.get_arg(0);
    create_collect_use(env, 2, || {
        match env.call("funcall", &[f] ) {
            Err(error) => {
                if let Some(&Signal { ref data, .. }) = error.downcast_ref::<ErrorKind>() {
                    unsafe {
                        return Ok(data.value(env))
                    }
                }
                Err(error)
            },
            v => v,
        }
    }, print)
}

pub fn init(env: &Env) -> Result<()> {
    emacs_export_functions! {
        env, *MODULE_PREFIX, {
            "gc-after-new-string" => (gc_after_new_string, 0..0),
            "gc-after-uninterning" => (gc_after_uninterning, 0..0),
            "gc-after-retrieving" => (gc_after_retrieving, 0..0),
            "gc-after-catching-1" => (gc_after_catching_1, 1..1),
        }
    }

    Ok(())
}
