use std::{error, fmt, num};

#[derive(Debug)]
pub enum Error {
    ParsingIncomplete,
    InternalError(Box<error::Error>),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl error::Error for Error {}

impl From<num::ParseIntError> for Error {
    fn from(o: num::ParseIntError) -> Error {
        Error::InternalError(box o)
    }
}
