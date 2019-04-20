macro_rules! evaluation_error {
    () => ({
        panic!("Unhandled syntax error")
    });
    ($msg:expr) => ({
        eprintln!($msg);
        panic!();
    });
    ($fmt:expr, $($arg:tt)+) => ({
        eprintln!($fmt, $($arg)+);
        panic!();
    });
}
