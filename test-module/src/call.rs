use crate::*;

#[defun]
fn list(env: &Env, n: u16) -> Result<Value> {
    let x = "x";
    let y = 1;
    let z = true;

    let vx = x.into_lisp(env)?;
    let vy = y.into_lisp(env)?;
    let vz = z.into_lisp(env)?;

    let list = env.intern("list")?;
    let list_subr = env.call("symbol-function", [list])?;

    // Passing statically-sized slices and arrays.
    env.call("list", &[])?;
    env.call("list", [])?;
    env.call("list", &[vx, vy, vz])?;
    env.call("list", [vx, vy, vz])?;
    env.call("list", [vx])?;

    // Passing tuples.
    env.call("list", (x, y, z))?;

    // Calling by symbol, subr, owned string.
    env.call(list, [])?;
    env.call(list_subr, (x, y, z))?;
    env.call(String::from("list"), [])?;

    // Passing dynamically-sized slice.
    let mut ints = vec![];
    for i in 0..n {
        ints.push(i.into_lisp(env)?);
    }
    env.call(list, ints.as_slice())
}

#[defun]
fn value<'e>(function: Value<'e>, arg: Value) -> Result<Value<'e>> {
    function.call([arg])
}

#[defun]
fn mapc_vec(function: Value, vector: emacs::Vector) -> Result<()> {
    for i in 0..vector.size()? as usize {
        let elem: Value = vector.get(i)?;
        function.call((i, elem))?;
    }
    Ok(())
}
