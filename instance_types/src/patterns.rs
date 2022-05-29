#[derive(Clone, Copy)]
pub enum PatternReachability {
    Reachable,
    Certain,
    Unreachable,
}
