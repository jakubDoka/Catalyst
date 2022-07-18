#![feature(let_chains)]

mod cycle_detector;

pub use cycle_detector::{
    CycleDetector, CycleDetectorNode, ProjectedCycleDetector, ProjectedCycleDetectorNode,
};
