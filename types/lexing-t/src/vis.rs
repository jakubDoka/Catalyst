use std::fmt;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
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
