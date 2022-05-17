use std::str::FromStr;

use storage::ReservedValue;

pub struct Macro {
    pub from: Stage,
    pub to: Stage,
}

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Stage {
    Source = 0,
    Tokenization,
    Parsing,
    Typec,
    Instance,
    Gen,

    Reserved,
}

impl Stage {
    pub fn can_bridge_to(self, other: Stage) -> bool {
        self <= other
    }
}

impl FromStr for Stage {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "source" => Ok(Stage::Source),
            "tokenization" => Ok(Stage::Tokenization),
            "parsing" => Ok(Stage::Parsing),
            "typec" => Ok(Stage::Typec),
            "instance" => Ok(Stage::Instance),
            "gen" => Ok(Stage::Gen),
            _ => Err(()),
        }
    }
}

impl ReservedValue for Macro {
    fn reserved_value() -> Self {
        Macro {
            from: Stage::Reserved,
            to: Stage::Reserved,
        }
    }

    fn is_reserved_value(&self) -> bool {
        self.from == Stage::Reserved
    }
}