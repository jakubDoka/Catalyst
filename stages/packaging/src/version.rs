use std::str::FromStr;

pub struct Version {
    major: u32,
    _minor: u32,
    _patch: u32,
}

impl Version {
    pub fn major(&self) -> u32 {
        self.major
    }
}

impl FromStr for Version {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s.split('.').map(|s| s.parse::<u32>());

        let (
            Some(Ok(major)),
            Some(Ok(minor)),
            Some(Ok(patch)),
        ) = (
            parts.next(),
            parts.next(),
            parts.next(),
        ) else {
            return Err(());
        };

        Ok(Version {
            major,
            _minor: minor,
            _patch: patch,
        })
    }
}
