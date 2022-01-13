use std::fmt;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(PartialEq, Eq, Debug)]
pub struct Error {
    details: String
}

impl Error {
    pub fn new(msg: String) -> Error {
        Self{details: msg}
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.details)
    }
}

impl std::error::Error for Error {
    fn description(&self) -> &str {
        &self.details
    }
}
