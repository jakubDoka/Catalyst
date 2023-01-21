use std::fmt;

use bytecheck::CheckBytes;
use rkyv::{Archive, Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Archive, Serialize, Deserialize)]
#[archive_attr(derive(CheckBytes))]
pub enum Vis {
    Pub,
    Priv,
}

impl fmt::Display for Vis {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Vis::Pub => f.write_str("pub"),
            Vis::Priv => f.write_str("priv"),
        }
    }
}
