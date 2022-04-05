use std::collections::{HashSet, HashMap};


pub struct CmdInput {
    args: Vec<String>,
    flags: HashSet<String>,
    fields: HashMap<String, String>,
}

impl CmdInput {
    pub fn new() -> Self {
        let mut ci = CmdInput {
            args: Vec::new(),
            flags: HashSet::new(),
            fields: HashMap::new(),
        };

        ci.parse();

        ci
    }

    pub fn enabled(&self, flag: &str) -> bool {
        self.flags.contains(flag)
    }

    pub fn field(&self, field: &str) -> Option<&str> {
        self.fields.get(field).map(|s| s.as_str())
    }

    pub fn args(&self) -> &[String] {
        &self.args
    }

    fn parse(&mut self) {
        let mut input = std::env::args().skip(1);
        let mut field = None;
        while let Some(mut arg) = input.next() {
            // collect the key of key value pair
            if arg.starts_with("--") {
                arg.replace_range(0..2, "");
                field = Some(arg);
                continue;
            }

            // complete the string
            if arg.starts_with('"') {
                while let Some(c) = input.next() {
                    arg.push_str(&c);
                    if c.ends_with('"') {
                        arg.pop();
                        break;
                    }
                }
                arg.remove(0);
            }

            // collect the value of key value pair
            if let Some(field) = field.take() {
                self.fields.insert(field, arg);
                continue;
            }

            // collect the flags
            if arg.starts_with('-') {
                self.flags.insert(arg);
                continue;
            }

            // collect the arguments
            self.args.push(arg);
        }

        // consider this a behavior
        if let Some(field) = field {
            self.fields.insert(field, "".into());
        }
    }
}