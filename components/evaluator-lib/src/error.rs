macro_rules! evaluation_error {
    () => ({
        panic!("Unhandled syntax error")
    });
    ($msg:expr) => ({
        eprintln!($msg);
        ::std::process::exit(0x1);
    });
    ($fmt:expr, $($arg:tt)+) => ({
        eprintln!($fmt, $($arg)+);
        ::std::process::exit(0x1);
    });
}
