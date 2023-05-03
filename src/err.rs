pub trait Unwrappable {
    type Inner;
    fn ok(&self) -> bool;
    fn unwrap(self) -> Self::Inner;
}

impl<T> Unwrappable for Option<T> {
    type Inner = T;
    fn ok(&self) -> bool { self.is_some() }
    fn unwrap(self) -> T { self.unwrap() }
}

impl<T, E: std::fmt::Debug> Unwrappable for Result<T, E> {
    type Inner = T;
    fn ok(&self) -> bool { self.is_ok() }
    fn unwrap(self) -> T { self.unwrap() }
}

macro_rules! err {
    ($val:expr, $err:expr) => {{
        let t = $val;
        if Unwrappable::ok(&t) { Unwrappable::unwrap(t) }
        else { eprintln!("{}", $err); std::process::exit(1) }
    }}
}

pub(crate) use err as err;
